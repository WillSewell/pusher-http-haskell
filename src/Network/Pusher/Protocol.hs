{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Network.Pusher.Protocol
-- Description : Types representing Pusher messages
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : experimental
--
-- Types representing the JSON format of Pusher messages.
--
-- There are also types for query string parameters.
module Network.Pusher.Protocol
  ( ChannelInfo (..),
    ChannelInfoAttributes (..),
    ChannelInfoQuery (..),
    ChannelsInfo (..),
    ChannelsInfoQuery (..),
    ChannelsInfoAttributes (..),
    FullChannelInfo (..),
    User (..),
    Users (..),
    ToURLParam,
    toURLParam,
  )
where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Pusher.Data (Channel, parseChannel)
import Network.Pusher.Internal.Util (failExpectObj)

-- | Types that can be serialised to a querystring parameter value.
class ToURLParam a where
  toURLParam :: a -> T.Text

-- | Enumeration of the attributes that can be queried about multiple channels.
data ChannelsInfoAttributes
  = ChannelsUserCount
  deriving (Eq, Generic)

instance ToURLParam ChannelsInfoAttributes where
  toURLParam ChannelsUserCount = "user_count"

instance Hashable ChannelsInfoAttributes

-- | A set of requested 'ChannelsInfoAttributes'.
newtype ChannelsInfoQuery
  = ChannelsInfoQuery (HS.HashSet ChannelsInfoAttributes)
  deriving (ToURLParam)

-- | Enumeration of the attributes that can be queried about a single channel.
data ChannelInfoAttributes
  = ChannelUserCount
  | ChannelSubscriptionCount
  deriving (Eq, Generic)

instance ToURLParam ChannelInfoAttributes where
  toURLParam ChannelUserCount = "user_count"
  toURLParam ChannelSubscriptionCount = "subscription_count"

instance Hashable ChannelInfoAttributes

-- | A set of requested 'ChannelInfoAttributes'.
newtype ChannelInfoQuery
  = ChannelInfoQuery (HS.HashSet ChannelInfoAttributes)
  deriving (ToURLParam)

instance ToURLParam a => ToURLParam (HS.HashSet a) where
  toURLParam hs = T.intercalate "," $ toURLParam <$> HS.toList hs

-- | A map of channels to their 'ChannelInfo'. The result of querying channel
--  info from multuple channels.
newtype ChannelsInfo
  = ChannelsInfo (HM.HashMap Channel ChannelInfo)
  deriving (Eq, Show)

instance A.FromJSON ChannelsInfo where
  parseJSON (A.Object v) = do
    chansV <- v .: "channels"
    case chansV of
      A.Object cs ->
        -- Need to go to and from list in order to map (parse) the keys
        ChannelsInfo . HM.fromList
          <$> mapM
            (\(channel, info) -> (parseChannel channel,) <$> A.parseJSON info)
            (HM.toList cs)
      v1 -> failExpectObj v1
  parseJSON v2 = failExpectObj v2

-- | The possible returned channel attributes when multiple when multiple
--  channels are queried.
newtype ChannelInfo
  = ChannelInfo
      { channelInfoUserCount :: Maybe Int
      }
  deriving (Eq, Show)

instance A.FromJSON ChannelInfo where
  parseJSON (A.Object v) = ChannelInfo <$> v .:? "user_count"
  parseJSON v = failExpectObj v

-- | The possible values returned by a query to a single channel.
data FullChannelInfo
  = FullChannelInfo
      { fullChannelInfoOccupied :: Bool,
        fullChannelInfoUserCount :: Maybe Int,
        fullChannelInfoSubCount :: Maybe Int
      }
  deriving (Eq, Show)

instance A.FromJSON FullChannelInfo where
  parseJSON (A.Object v) =
    FullChannelInfo <$> v .: "occupied" <*> v .:? "user_count"
      <*> v .:? "subscription_count"
  parseJSON v = failExpectObj v

-- | A list of users returned by querying for users in a presence channel.
newtype Users
  = Users [User]
  deriving (Eq, Show)

instance A.FromJSON Users where
  parseJSON (A.Object v) = do
    users <- v .: "users"
    Users <$> A.parseJSON users
  parseJSON v = failExpectObj v

-- | The data about a user returned when querying for users in a presence
--  channel.
newtype User
  = User
      { userID :: T.Text
      }
  deriving (Eq, Show)

instance A.FromJSON User where
  parseJSON (A.Object v) = User <$> v .: "id"
  parseJSON v = failExpectObj v
