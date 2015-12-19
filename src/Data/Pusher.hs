{-|
Module      : Data.Pusher
Description : Data structure to store Pusher config
Copyright   : (c) Will Sewell, 2015
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

You must create an instance of this datatype with your particular Pusher app
credentials in order to run the main API functions.
-}
module Data.Pusher
  ( Pusher(..)
  , Credentials(..)
  , getPusher
  , getPusherWithHost
  , getPusherWithConnManager
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T

import Network.Pusher.Internal.Util (failExpectObj, show')

-- |All the required configuration needed to interact with the API.
data Pusher = Pusher
  { pusherHost :: T.Text
  , pusherPath :: T.Text
  , pusherCredentials :: Credentials
  , pusherConnectionManager :: Manager
  }

-- |The credentials for the current app.
data Credentials = Credentials
  { credentialsAppID :: Integer
  , credentialsAppKey :: B.ByteString
  , credentialsAppSecret :: B.ByteString
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
