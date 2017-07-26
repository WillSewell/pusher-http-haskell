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
  -- * Pusher config data type
  ( AppID
  , AppKey
  , AppSecret
  , Pusher(..)
  , Credentials(..)
  , Cluster(..)
  , clusterMt1
  , clusterEu
  , clusterAp1
  , clusterAp2
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
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Data.Foldable (asum)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client
       (Manager, defaultManagerSettings, newManager)

import Network.Pusher.Internal.Util
       (failExpectObj, failExpectStr, show')

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
  , credentialsCluster :: Maybe Cluster
  }

instance A.FromJSON Credentials where
  parseJSON (A.Object v) =
    Credentials <$> v .: "app-id" <*> (encodeUtf8 <$> v .: "app-key") <*>
    (encodeUtf8 <$> v .: "app-secret") <*>
    v .:? "app-cluster"
  parseJSON v2 = failExpectObj v2

-- | The cluster the current app resides on. Common clusters include: mt1,eu,ap1,ap2
newtype Cluster = Cluster
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

-- |Use this to get an instance Pusher. This will fill in the host and path
-- automatically.
getPusher
  :: MonadIO m
  => Credentials -> m Pusher
getPusher cred = do
  connManager <- getConnManager
  return $ getPusherWithConnManager connManager Nothing cred

-- |Get a Pusher instance that uses a specific API endpoint.
getPusherWithHost
  :: MonadIO m
  => T.Text -> Credentials -> m Pusher
getPusherWithHost apiHost cred = do
  connManager <- getConnManager
  return $ getPusherWithConnManager connManager (Just apiHost) cred

-- |Get a Pusher instance with a given connection manager. This can be useful
-- if you want to share a connection with your application code.
getPusherWithConnManager :: Manager -> Maybe T.Text -> Credentials -> Pusher
getPusherWithConnManager connManager apiHost cred =
  let path = "/apps/" <> show' (credentialsAppID cred) <> "/"
      mCluster = credentialsCluster cred
  in Pusher
     { pusherHost = fromMaybe (mkHost mCluster) apiHost
     , pusherPath = path
     , pusherCredentials = cred
     , pusherConnectionManager = connManager
     }

-- |Given a possible cluster, return the corresponding host
mkHost :: Maybe Cluster -> T.Text
mkHost mCluster =
  case mCluster of
    Nothing -> "http://api.pusherapp.com"
    Just c -> "http://api" <> renderClusterSuffix c <> ".pusher.com"

getConnManager
  :: MonadIO m
  => m Manager
getConnManager = liftIO $ newManager defaultManagerSettings

type ChannelName = T.Text

-- |The possible types of Pusher channe.
data ChannelType
  = Public
  | Private
  | Presence
  deriving (Eq, Generic, Show)

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
parseChannel chan
  -- Attempt to parse it as a private or presence channel; default to public
 =
  fromMaybe
    (Channel Public chan)
    (asum [parseChanAs Private, parseChanAs Presence])
  where
    parseChanAs chanType =
      let split = T.splitOn (renderChannelPrefix chanType) chan
    -- If the prefix appears at the start, then the first element will be empty
      in if length split > 1 && T.null (head split)
           then Just $ Channel chanType (T.concat $ tail split)
           else Nothing

type Event = T.Text

type EventData = T.Text

type SocketID = T.Text
