{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Network.Pusher.Data
Description : Data structure representing Pusher concepts and config
Copyright   : (c) Will Sewell, 2016
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

You must create an instance of the Pusher datatype with your particular Pusher
app credentials in order to run the main API functions.

The other types represent Pusher channels and Pusher event fields.
-}
module Network.Pusher.Data
  (
  -- * Pusher config data type
    AppID
  , AppKey
  , AppSecret
  , Pusher(..)
  , Credentials(..)
  , getPusher
  , getPusherWithHost
  , getPusherWithConnManager
  -- * Channels
  , Channel(..)
  , ChannelName
  , ChannelType(..)
  , renderChannel
  , renderChannelPrefix
  , parseChannel
  -- Events
  , Event
  , EventData
  , SocketID
  -- Notifications
  , Notification
  , WebhookLevel(..)
  , parseInterest
  )

  where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:), (.:?), (.=))
import Data.Char (isAlphaNum)
import Data.Default (Default(..))
import Data.Foldable (asum)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Vector ((!))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import Network.Pusher.Internal.Util (failExpect, failExpectObj, show')
import Network.Pusher.Internal.Payloads (Payload, PayloadSource(..), payloadObject, Apns, Gcm, Fcm)

type AppID = Integer

type AppKey = B.ByteString

type AppSecret = B.ByteString

-- |All the required configuration needed to interact with the API.
data Pusher = Pusher
  { pusherHost :: T.Text
  , pusherPath :: T.Text
  , pusherCredentials :: Credentials
  , pusherConnectionManager :: Manager
  }

-- |The credentials for the current app.
data Credentials = Credentials
  { credentialsAppID :: AppID
  , credentialsAppKey :: AppKey
  , credentialsAppSecret :: AppSecret
  }

instance A.FromJSON Credentials where
  parseJSON (A.Object v) = Credentials
    <$> v .: "app-id"
    <*> (encodeUtf8 <$> v .: "app-key")
    <*> (encodeUtf8 <$> v .: "app-secret")
  parseJSON v2 = failExpectObj v2

-- |Use this to get an instance Pusher. This will fill in the host and path
-- automatically.
getPusher :: MonadIO m => Credentials -> m Pusher
getPusher cred = do
    connManager <- getConnManager
    return $ getPusherWithConnManager connManager Nothing cred

-- |Get a Pusher instance that uses a specific API endpoint.
getPusherWithHost :: MonadIO m => T.Text -> Credentials -> m Pusher
getPusherWithHost apiHost cred = do
  connManager <- getConnManager
  return $ getPusherWithConnManager connManager (Just apiHost) cred

-- |Get a Pusher instance with a given connection manager. This can be useful
-- if you want to share a connection with your application code.
getPusherWithConnManager :: Manager -> Maybe T.Text -> Credentials -> Pusher
getPusherWithConnManager connManager apiHost cred =
  let path = "/apps/" <> show' (credentialsAppID cred) <> "/" in
  Pusher
    { pusherHost = fromMaybe "http://api.pusherapp.com" apiHost
    , pusherPath = path
    , pusherCredentials = cred
    , pusherConnectionManager = connManager
    }

getConnManager :: MonadIO m => m Manager
getConnManager = liftIO $ newManager defaultManagerSettings

type ChannelName = T.Text

-- |The possible types of Pusher channe.
data ChannelType = Public | Private | Presence deriving (Eq, Generic, Show)

instance Hashable ChannelType

renderChannelPrefix :: ChannelType -> T.Text
renderChannelPrefix Public = ""
renderChannelPrefix Private = "private-"
renderChannelPrefix Presence = "presence-"

-- |The channel name (not including the channel type prefix) and its type.
data Channel = Channel
  { channelType :: ChannelType
  , channelName :: ChannelName
  } deriving (Eq, Generic, Show)

instance Hashable Channel

renderChannel :: Channel -> T.Text
renderChannel (Channel cType cName) = renderChannelPrefix cType <> cName

-- |Convert string representation, e.g. private-chan into the datatype
parseChannel :: T.Text -> Channel
parseChannel chan =
  -- Attempt to parse it as a private or presence channel; default to public
  fromMaybe
    (Channel Public chan)
    (asum [parseChanAs Private,  parseChanAs Presence])
 where
  parseChanAs chanType =
    let split = T.splitOn (renderChannelPrefix chanType) chan in
    -- If the prefix appears at the start, then the first element will be empty
    if length split > 1 && T.null (head split) then
      Just $ Channel chanType (T.concat $ tail split)
    else
      Nothing

type Event = T.Text

type EventData = T.Text

type SocketID = T.Text

data Notification = Notification
  { notificationInterests :: Interest
  , notificationWebhookUrl :: Maybe T.Text
  , notificationWebhookLevel :: Maybe WebhookLevel
  , notificationPayload :: Payload
  } deriving (Eq, Show)

instance A.ToJSON Notification where
  toJSON (Notification interests webhookUrl webhookLevel payload) =
    A.Object $ mconcat
      [ payloadObject payload
      , HM.fromList
          [ "interests" .= interests
          , "webhook_url" .= webhookUrl
          , "webhook_level" .= webhookLevel
          ]
      ]

instance A.FromJSON Notification where
  parseJSON (A.Object v) = do
    interests' <- v .: "interests"
    webhookUrl' <- v .:? "webhook_url"
    webhookLevel' <- v .:? "webhook_level"
    apns <- v .:? "apns"
    gcm <- v .:? "gcm"
    fcm <- v .:? "fcm"
    let
      payload' = mconcat . catMaybes $
        [ renderPayload <$> (apns :: Maybe Apns)
        , renderPayload <$> (gcm :: Maybe Gcm)
        , renderPayload <$> (fcm :: Maybe Fcm)
        ]
    return $ Notification interests' webhookUrl' webhookLevel' payload'
  parseJSON v = failExpectObj v

instance Default Notification where
  def = Notification
    { notificationInterests = def
    , notificationWebhookUrl = Nothing
    , notificationWebhookLevel = Nothing
    , notificationPayload = def
    }

mkEmptyNotification
  :: Interest
  -> Maybe T.Text
  -> Maybe WebhookLevel
  -> Notification
mkEmptyNotification interests' webhookUrl' webhookLevel' =
  Notification interests' webhookUrl' webhookLevel' mempty

mkNotification
  :: Interest
  -> Maybe T.Text
  -> Maybe WebhookLevel
  -> Payload
  -> Notification
mkNotification = Notification

addPayload :: Notification -> Payload -> Notification
addPayload notification newPayload =
  notification
    { notificationPayload = notificationPayload notification <> newPayload }

newtype Interest = Interest T.Text deriving (Eq, Show)

instance Default Interest where
  def = Interest "default"

-- Pusher API requires this be passed in as a single-item array
instance A.ToJSON Interest where
  toJSON (Interest a) = A.toJSON [a]

instance A.FromJSON Interest where
  parseJSON arr@(A.Array v) =
    if V.length v == 1
      then
        let (A.String interest) = v ! 0
        in pure . Interest $ interest
      else failExpect "JSON array of length 1" arr
  parseJSON v = failExpect "JSON array of length 1" v

-- |Convert string representation into an Interest
-- Falls back to default instance in the case of a parse failure
-- Each interest name can be up to 164 characters. Each character
-- in the name must be an ASCII upper- or lower-case letter,
-- a number, or one of _=@,.;.
parseInterest :: T.Text -> Interest
parseInterest interest =
  if T.all isAllowed interest && T.length interest <= 164
    then Interest interest
    else def
  where
    isAllowed c = isAlphaNum c || c `elem` ("_=@,.;" :: String)

data WebhookLevel = Info | Debug deriving (Eq, Show)

instance A.ToJSON WebhookLevel where
  toJSON Info = "INFO"
  toJSON Debug = "DEBUG"

instance A.FromJSON WebhookLevel where
  parseJSON (A.String "INFO") = pure Info
  parseJSON (A.String "DEBUG") = pure Debug
  parseJSON v = failExpectObj v
