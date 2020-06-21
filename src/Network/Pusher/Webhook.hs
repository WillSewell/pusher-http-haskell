module Network.Pusher.Webhook
  ( Webhooks(..)
  , WebhookEv(..)
  , WebhookPayload(..)
  , parseAppKeyHdr
  , parseAuthSignatureHdr
  , parseWebhooksBody
  , verifyWebhooksBody
  , parseWebhookPayloadWith
  ) where

import qualified Crypto.Hash as HASH
import qualified Crypto.MAC.HMAC as HMAC
import Data.Aeson ((.:))
import qualified Data.Aeson as A
import Data.ByteArray (convert)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (toLower)
import Data.Function (on)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Network.Pusher.Data
       (Channel(..))
import Network.Pusher.Internal.Util
import Network.Pusher.Protocol (User(..))

-- |A Webhook is received by POST request from Pusher to notify your server of
-- a number of 'WebhookEv's. Multiple events are received under the same
-- timestamp if batch events is enabled.
data Webhooks = Webhooks
  { timeMs :: Word64
  , webhookEvs :: [WebhookEv]
  } deriving (Eq, Show)

instance A.FromJSON Webhooks where
  parseJSON o =
    case o of
      A.Object v ->
        Webhooks <$> v .: "time_ms" <*> v .: "events"
      _ -> failExpectObj o

-- |A 'WebhookEv' is one of several events Pusher may send to your server in
-- response to events your users may trigger.
data WebhookEv
  -- |A channel has become occupied. There is > 1 subscriber.
  = ChannelOccupiedEv { onChannel :: Channel }
  -- |A channel has become vacated. There are 0 subscribers.
  | ChannelVacatedEv { onChannel :: Channel }
  -- |A new user has subscribed to a presence channel.
  | MemberAddedEv { onChannel :: Channel
                  , withUser :: User }
  -- |A user has unsubscribed from a presence channel.
  | MemberRemovedEv { onChannel :: Channel
                    , withUser :: User }
  -- |A client has sent a named client event with some json body. They have a
  -- 'SocketID' and a 'User' if they were in a presence channel.
  | ClientEv { onChannel :: Channel
             , clientEvName :: T.Text
             , clientEvBody :: Maybe A.Value
             , withSocketId :: T.Text
             , withPossibleUser :: Maybe User }
  deriving (Eq, Show)

instance A.FromJSON WebhookEv where
  parseJSON o =
    case o of
      A.Object v -> do
        name <- v .: "name"
        case name :: T.Text of
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

data WebhookPayload = WebhookPayload {
    xPusherKey :: B.ByteString
  -- ^Authentication header. The oldest active token is used, identified by
  -- this key.
  , xPusherSignature :: B.ByteString
  -- ^A HMAC SHA256 formed by signing the payload with the tokens secret.
  , webhooks :: Webhooks
  } deriving (Eq, Show)

-- |Given a HTTP Header and its associated value, parse an app key.
parseAppKeyHdr :: BC.ByteString -> BC.ByteString -> Maybe B.ByteString
parseAppKeyHdr key value
  | on (==) (BC.map toLower) key "X-Pusher-Key" = Just value
  | otherwise = Nothing

-- |Given a HTTP Header and its associated value, parse an auth signature.
parseAuthSignatureHdr :: BC.ByteString -> BC.ByteString -> Maybe B.ByteString
parseAuthSignatureHdr key value
  | on (==) (BC.map toLower) key "X-Pusher-Signature" = Just value
  | otherwise = Nothing

-- |Given a HTTP body, parse the contained webhooks.
parseWebhooksBody :: BC.ByteString -> Maybe Webhooks
parseWebhooksBody = A.decode . fromStrict

-- |Does a webhook body hash with our secret key to the given signature?
verifyWebhooksBody :: B.ByteString -> B.ByteString -> BC.ByteString -> Bool
verifyWebhooksBody appSecret authSignature body =
  let actualSignature =
        B16.encode $ convert (HMAC.hmac appSecret body :: HMAC.HMAC HASH.SHA256)
  in authSignature == actualSignature

-- |Given a list of http header key:values, a http body and a lookup function
-- for an apps secret, parse and validate a  potential webhook payload.
parseWebhookPayloadWith ::
     (B.ByteString -> Maybe B.ByteString)
  -> [(BC.ByteString, BC.ByteString)]
  -> BC.ByteString
  -> Maybe WebhookPayload
parseWebhookPayloadWith lookupKeysSecret headers body = do
  appKey <- listToMaybe . mapMaybe (uncurry parseAppKeyHdr) $ headers
  authSignature <- listToMaybe . mapMaybe (uncurry parseAuthSignatureHdr) $ headers
  appSecret <- lookupKeysSecret appKey
  () <-
    if verifyWebhooksBody appSecret authSignature body
      then Just ()
      else Nothing
  whs <- parseWebhooksBody body
  Just $ WebhookPayload appKey authSignature whs
