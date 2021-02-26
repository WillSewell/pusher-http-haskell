-- |
-- Module      : Network.Pusher.Data
-- Description : Data structures representing Channels concepts and settings
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : stable
module Network.Pusher.Data
  ( Settings (..),
    defaultSettings,
    Token (..),
    Address (..),
    Pusher (..),
    newPusher,
    newPusherWithConnManager,
    Event (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16, Word32)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Pusher.Internal.Util (show')

-- | All the required configuration needed to interact with the API.
data Settings
  = Settings
      { pusherAddress :: Address,
        pusherAppID :: Word32,
        pusherToken :: Token,
        pusherUseTLS :: Bool
      }

instance A.FromJSON Settings where
  parseJSON =
    A.withObject "Settings" $ \v -> do
      cluster <- (encodeUtf8 <$>) <$> v .:? "cluster"
      host <- (encodeUtf8 <$>) <$> v .:? "host"
      port <- v .:? "port"
      let address = case (cluster, host, port) of
            (Just c, Nothing, Nothing) -> Just $ Cluster c
            (Nothing, Just h, Just p) -> Just $ HostPort h p
            (Nothing, Nothing, Nothing) -> Nothing
            _ -> fail "`cluster` is mutually exclusive with `host` and `port`"
      appID <- v .: "app_id"
      token <- v .: "token"
      useTLS <- v .:? "use_tls"
      let settings =
            defaultSettings
              { pusherAppID = appID,
                pusherToken = token
              }
      pure $ setOptionals address useTLS settings
    where
      setOptionals maybeAddress maybeUseTLS =
        setAddress maybeAddress . setUseTLS maybeUseTLS
      setAddress (Just address) settings = settings {pusherAddress = address}
      setAddress Nothing settings = settings
      setUseTLS (Just useTLS) settings = settings {pusherUseTLS = useTLS}
      setUseTLS Nothing settings = settings

-- | A convenient way of creating an instance of 'Settings'. Another
-- benefit is it prevents breaking changes when fields are added to
-- 'Settings', see <https://www.yesodweb.com/book/settings-types>.You
-- must set 'pusherAppID' and 'pusherToken'. Currently 'pusherAddress'
-- defaults to the @mt1@ cluster.
--
-- Example:
--
-- @
-- defaultSettings
--   { 'pusherAppID' = 123,
--     'pusherToken' = 'Token' { 'tokenKey' = "key", 'tokenSecret' "secret" }
--   }
-- @
defaultSettings :: Settings
defaultSettings =
  Settings
    { pusherAddress = Cluster "mt1",
      pusherAppID = 1,
      pusherToken = Token "" "",
      pusherUseTLS = True
    }

-- | A Channels key and secret pair for a particular app.
data Token
  = Token
      { tokenKey :: B.ByteString,
        tokenSecret :: B.ByteString
      }

instance A.FromJSON Token where
  parseJSON =
    A.withObject "Token" $ \v -> do
      key <- encodeUtf8 <$> v .: "key"
      secret <- encodeUtf8 <$> v .: "secret"
      pure $ Token key secret

-- | Typically you will want to connect directly to a standard Pusher Channels
-- 'Cluster'.
data Address
  = -- | The cluster the current app resides on. Common clusters include:
    -- @mt1@, @eu@, @ap1@, @ap2@.
    Cluster B.ByteString
  | -- | Used to connect to a raw address:port.
    HostPort B.ByteString Word16

-- | The core handle to the Pusher API.
data Pusher
  = Pusher
      { pUseTLS :: Bool,
        pHost :: B.ByteString,
        pPort :: Word16,
        pPath :: B.ByteString,
        pToken :: Token,
        pConnectionManager :: Manager
      }

-- | Use this to get a Pusher instance.
newPusher :: MonadIO m => Settings -> m Pusher
newPusher settings = do
  connManager <- newTlsManager
  return $ newPusherWithConnManager connManager settings

-- | Get a Pusher instance with a given connection manager. This can be useful
--  if you want to share a connection with your application code.
newPusherWithConnManager :: Manager -> Settings -> Pusher
newPusherWithConnManager connectionManager settings =
  let (host, port) = case pusherAddress settings of
        HostPort h p -> (h, p)
        Cluster c -> ("api-" <> c <> ".pusher.com", if pusherUseTLS settings then 443 else 80)
      path = "/apps/" <> show' (pusherAppID settings) <> "/"
   in Pusher
        { pUseTLS = pusherUseTLS settings,
          pHost = host,
          pPort = port,
          pPath = path,
          pToken = pusherToken settings,
          pConnectionManager = connectionManager
        }

data Event
  = Event
      { -- | Channel to trigger on.
        eventChannel :: T.Text,
        -- | Event name.
        eventName :: T.Text,
        -- | Event data. Often encoded JSON.
        eventData :: T.Text,
        -- | An optional socket ID of a connection you wish to exclude.
        eventSocketId :: Maybe T.Text
      }

instance A.ToJSON Event where
  toJSON (Event channel name dat socketId) =
    A.object $
      [ ("name", A.String name),
        ("channel", A.String channel),
        ("data", A.String dat)
      ]
        ++ maybeToList (fmap (\sID -> ("socket_id", A.String sID)) socketId)
