{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Network.Pusher.Data
-- Description : Data structure representing Pusher Channels concepts and config
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : experimental
--
-- You must create an instance of the Pusher datatype with your particular Pusher
-- Channels app credentials in order to run the main API functions.
--
-- The other types represent channels and event fields.
module Network.Pusher.Data
  ( -- * Pusher config data type
    Pusher (..),
    Credentials (..),
    Cluster (..),
    clusterMt1,
    clusterEu,
    clusterAp1,
    clusterAp2,
    getPusher,
    getPusherWithHost,
    getPusherWithConnManager,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Network.HTTP.Client
  ( Manager,
    defaultManagerSettings,
    newManager,
  )
import Network.Pusher.Internal.Util
  ( failExpectObj,
    failExpectStr,
    show',
  )

-- | All the required configuration needed to interact with the API.
data Pusher
  = Pusher
      { pusherHost :: T.Text,
        pusherPath :: T.Text,
        pusherCredentials :: Credentials,
        pusherConnectionManager :: Manager
      }

-- | The credentials for the current app.
data Credentials
  = Credentials
      { credentialsAppID :: Word32,
        credentialsAppKey :: B.ByteString,
        credentialsAppSecret :: B.ByteString,
        credentialsCluster :: Maybe Cluster
      }

instance A.FromJSON Credentials where
  parseJSON (A.Object v) =
    Credentials <$> v .: "app-id" <*> (encodeUtf8 <$> v .: "app-key")
      <*> (encodeUtf8 <$> v .: "app-secret")
      <*> v .:? "app-cluster"
  parseJSON v2 = failExpectObj v2

-- | The cluster the current app resides on. Common clusters include:
--  mt1,eu,ap1,ap2.
newtype Cluster
  = Cluster
      { clusterName :: T.Text
      }

clusterMt1, clusterEu, clusterAp1, clusterAp2 :: Cluster
clusterMt1 = Cluster "mt1"
clusterEu = Cluster "eu"
clusterAp1 = Cluster "ap1"
clusterAp2 = Cluster "ap2"

-- The possible cluster suffix given in a host name
renderClusterSuffix :: Cluster -> T.Text
renderClusterSuffix cluster = "-" <> clusterName cluster

instance A.FromJSON Cluster where
  parseJSON v =
    case v of
      A.String txt -> return . Cluster $ txt
      _ -> failExpectStr v

-- | Use this to get an instance Pusher. This will fill in the host and path
--  automatically.
getPusher :: MonadIO m => Credentials -> m Pusher
getPusher cred = do
  connManager <- getConnManager
  return $ getPusherWithConnManager connManager Nothing cred

-- | Get a Pusher instance that uses a specific API endpoint.
getPusherWithHost :: MonadIO m => T.Text -> Credentials -> m Pusher
getPusherWithHost apiHost cred = do
  connManager <- getConnManager
  return $ getPusherWithConnManager connManager (Just apiHost) cred

-- | Get a Pusher instance with a given connection manager. This can be useful
--  if you want to share a connection with your application code.
getPusherWithConnManager :: Manager -> Maybe T.Text -> Credentials -> Pusher
getPusherWithConnManager connManager apiHost cred =
  let path = "/apps/" <> show' (credentialsAppID cred) <> "/"
      mCluster = credentialsCluster cred
   in Pusher
        { pusherHost = fromMaybe (mkHost mCluster) apiHost,
          pusherPath = path,
          pusherCredentials = cred,
          pusherConnectionManager = connManager
        }

-- | Given a possible cluster, return the corresponding host.
mkHost :: Maybe Cluster -> T.Text
mkHost mCluster =
  case mCluster of
    Nothing -> "http://api.pusherapp.com"
    Just c -> "http://api" <> renderClusterSuffix c <> ".pusher.com"

getConnManager :: MonadIO m => m Manager
getConnManager = liftIO $ newManager defaultManagerSettings
