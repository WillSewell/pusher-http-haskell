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
module Data.Pusher (Pusher(..), Credentials(..), getPusher) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:))
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T

import Pusher.Util (failExpectObj)

-- |All the required configuration needed to interact with the API.
data Pusher = Pusher
  { pusher'host :: T.Text
  , pusher'path :: T.Text
  , pusher'credentials :: Credentials
  , pusher'connectionManager :: Manager
  }

-- |The credentials for the current app.
data Credentials = Credentials
  { credentials'appID :: Integer
  , credentials'appKey :: B.ByteString
  , credentials'appSecret :: B.ByteString
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
  let path = "/apps/" <> T.pack (show $ credentials'appID cred) <> "/"
  connManager <- liftIO $ newManager defaultManagerSettings
  return Pusher
    { pusher'host = "http://api.pusherapp.com"
    , pusher'path = path
    , pusher'credentials = cred
    , pusher'connectionManager = connManager
    }
