module Network.Pusher.Webhook
  ( Webhooks(..)
  , WebhookEv(..)
  , WebhookPayload(..)
  , parseWebhookPayloadReq
  ) where

import qualified Crypto.Hash as HASH
import qualified Crypto.MAC.HMAC as HMAC
import Data.Aeson ((.:))
import qualified Data.Aeson as A
import Data.ByteArray
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time (UTCTime(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Network.HTTP.Server as HTTP
import Network.Pusher.Data
       (Channel(..), SocketID, AppKey, AppSecret)
import Network.Pusher.Internal.Auth (AuthSignature)
import Network.Pusher.Internal.Util
import Network.Pusher.Protocol (User(..))

-- | A Webhook is received by POST request from Pusher to notify your server of
-- a number of 'WebhookEv'ents. Multiple events are received under the same
-- timestamp if batch events is enabled.
data Webhooks = Webhooks
  { timeMs :: UTCTime
  , webhookEvs :: [WebhookEv]
  } deriving (Eq, Show)

instance A.FromJSON Webhooks where
  parseJSON o =
    case o of
      A.Object v ->
        Webhooks <$> (_unOurTime <$> A.parseJSON o) <*> (v .: "events")
      _ -> failExpectObj o

-- Exists only so we can define our own FromJSON instance on NominalDiffTime.
-- This is useful because it didnt exist before a certain GHC version that we
-- support and allows us to avoid CPP and orphan instances.
newtype OurTime = OurTime
  { _unOurTime :: UTCTime
  }

-- posixSecondsToUTCTime <$>
instance A.FromJSON OurTime where
  parseJSON o =
    case o of
      A.Object v ->
        A.withScientific
          "NominalDiffTime"
          (pure . OurTime . posixSecondsToUTCTime . realToFrac) =<<
        v .: "time_ms"
      _ -> failExpectObj o

-- | A 'WebhookEv'ent is one of several events Pusher may send to your server
-- in response to events your users may trigger.
data WebhookEv
  -- | A Channel has become occupied. There is > 1 subscriber.
  = ChannelOccupiedEv { onChannel :: Channel}
  -- | A Channel has become vacated. There are 0 subscribers.
  | ChannelVacatedEv { onChannel :: Channel}
  -- | A new user has subscribed to a presence Channel.
  | MemberAddedEv { onChannel :: Channel
                 ,  withUser :: User}
  -- | A user has unsubscribed from a presence Channel.
  | MemberRemovedEv { onChannel :: Channel
                   ,  withUser :: User}
  -- | A client has sent a named client event with some json body. They have a
  -- SocketID and a User if they were in a presence Channel.
  | ClientEv { onChannel :: Channel
            ,  clientEvName :: Text
            ,  clientEvBody :: Maybe A.Value
            ,  withSocketId :: SocketID
            ,  withPossibleUser :: Maybe User}
  deriving (Eq, Show)

instance A.FromJSON WebhookEv where
  parseJSON o =
    case o of
      A.Object v -> do
        name <- v .: "name"
        case name :: Text of
          "channel_occupied" -> ChannelOccupiedEv <$> v .: "channel"
          "channel_vacated" -> ChannelVacatedEv <$> v .: "channel"
          "member_added" ->
            MemberAddedEv <$> v .: "channel" <*> (User <$> v .: "user_id")
          "member_removed" ->
            MemberRemovedEv <$> v .: "channel" <*> (User <$> v .: "user_id")
          "client_event" ->
            ClientEv <$> v .: "channel" <*> v .: "event" <*>
            (A.decode . LB.fromStrict . encodeUtf8 <$> v .: "data") <*>
            v .: "socket_id" <*>
            (fmap User <$> v .: "user_id")
          _ -> fail . ("Unknown client event. Got: " ++) . show $ o
      _ -> failExpectObj o

data WebhookPayload = WebhookPayload
   -- | Authentication header. The oldest active token is used, identified by
   -- this key.
  { xPusherKey :: AppKey
   -- | A HMAC SHA256 formed by signing the payload with the tokens secret.
  , xPusherSignature :: AuthSignature
  , webhooks :: Webhooks
  } deriving (Eq, Show)

-- Pass a HTTP Request, we've recieved and attempt to parse it as a WebhookPayload.
-- Will return Nothing if the request is malformed or if the auth string does not validate.
parseWebhookPayloadReq :: HTTP.Request B.ByteString
                       -> (AppKey -> Maybe AppSecret)
                       -> Maybe WebhookPayload
parseWebhookPayloadReq req lookupKeysSecret
  -- Per the Maybe Monad instance, any of these failing shortcircuits
  -- the computation.
 = do
  () <-
    if HTTP.rqMethod req == HTTP.POST
      then Just ()
      else Nothing
  pusherKey <- B.pack <$> HTTP.findHeader (HTTP.HdrCustom "X-Pusher-Key") req
  pusherSignature <-
    B.pack <$> HTTP.findHeader (HTTP.HdrCustom "X-Pusher-Signature") req
  -- Body must match the given hash where we use the given public key to lookup
  -- the secret private key.
  let body = HTTP.rqBody req
  pusherSecret <- lookupKeysSecret pusherKey
  () <-
    if verifyWebhookBody pusherSecret pusherSignature body
      then Just ()
      else Nothing
  ws <- A.decode $ fromStrict body
  return $ WebhookPayload pusherKey pusherSignature ws

-- Does a webhook body hash with our secret key to the given signature?
verifyWebhookBody :: AppSecret -> AuthSignature -> B.ByteString -> Bool
verifyWebhookBody appSecret authSignature body =
  let actualSignature =
        B16.encode $
        convert $ (HMAC.hmac appSecret body :: HMAC.HMAC HASH.SHA256)
  in authSignature == actualSignature
