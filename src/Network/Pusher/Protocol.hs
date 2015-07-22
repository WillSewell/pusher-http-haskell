{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Network.Pusher.Protocol
Description : Types representing Pusher messages
Copyright   : (c) Will Sewell, 2015
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

Types representing the JSON format of Pusher messages.

There are also types for query string parameters.
-}
module Network.Pusher.Protocol
  ( ChannelInfo(..)
  , ChannelInfoAttributes(..)
  , ChannelInfoQuery(..)
  , ChannelsInfo(..)
  , ChannelsInfoQuery(..)
  , ChannelsInfoAttributes(..)
  , Users
  , toURLParam
  ) where

import Data.Aeson ((.:), (.:?))
import Data.Hashable (Hashable, hashWithSalt)
import Control.Applicative ((<$>))
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Traversable as Traversable

import Network.Pusher.Util (failExpectObj)

-- |Types that can be serialised to a querystring parameter value.
class ToURLParam a where
  -- |Convert the data into a querystring parameter value.
  toURLParam :: a -> T.Text

-- |Enumeration of the attributes that can be queried about multiple channels.
data ChannelsInfoAttributes = ChannelsUserCount deriving Generic

instance ToURLParam ChannelsInfoAttributes where
  toURLParam ChannelsUserCount = "user_count"

instance Hashable ChannelsInfoAttributes

-- |A set of requested ChannelsInfoAttributes.
newtype ChannelsInfoQuery =
  ChannelsInfoQuery (HS.HashSet ChannelsInfoAttributes)
  deriving ToURLParam

-- |Enumeration of the attributes that can be queried about a single channel.
data ChannelInfoAttributes = ChannelUserCount | ChannelSubscriptionCount
  deriving Generic

instance ToURLParam ChannelInfoAttributes where
  toURLParam ChannelUserCount = "user_count"
  toURLParam ChannelSubscriptionCount = "subscription_count"

instance Hashable ChannelInfoAttributes

-- |A set of requested ChannelInfoAttributes.
newtype ChannelInfoQuery = ChannelInfoQuery (HS.HashSet ChannelInfoAttributes)
  deriving ToURLParam


instance ToURLParam a => ToURLParam (HS.HashSet a) where
  toURLParam hs = T.concat $ toURLParam <$> HS.toList hs

-- |A map of channel names to their ChannelInfo. The result of querying channel
-- info from multuple channels.
newtype ChannelsInfo =
  ChannelsInfo (HM.HashMap T.Text ChannelInfo)
  deriving Show

instance A.FromJSON ChannelsInfo where
  parseJSON (A.Object v) = do
    chansV <- v .: "channels"
    case chansV of
      A.Object cs ->
        ChannelsInfo <$> Traversable.sequence (HM.map A.parseJSON cs)
      v1 -> failExpectObj v1
  parseJSON v2 = failExpectObj v2

-- |A set of returned channel attributes for a single channel.
newtype ChannelInfo =
  ChannelInfo (HS.HashSet ChannelInfoAttributeResp)
  deriving Show

instance A.FromJSON ChannelInfo where
  parseJSON (A.Object v) = do
    maybeUserCount <- v .:? "user_count"
    return $ ChannelInfo $ maybe
      HS.empty
      (HS.singleton . UserCountResp)
      maybeUserCount
  parseJSON v = failExpectObj v

-- |An enumeration of possible returned channel attributes. These now have
-- associated values.
data ChannelInfoAttributeResp = UserCountResp Int deriving Show

instance Hashable ChannelInfoAttributeResp where
  hashWithSalt salt (UserCountResp count) = hashWithSalt salt count

-- |A list of users returned by querying for users in a presence channel.
newtype Users = Users [User] deriving Show

instance A.FromJSON Users where
  parseJSON (A.Object v) = do
    users <- v .: "users"
    Users <$> A.parseJSON users
  parseJSON v = failExpectObj v

-- |The data about a user returned when querying for users in a presence channel.
data User = User { userID :: T.Text } deriving Show

instance A.FromJSON User where
  parseJSON (A.Object v) = User <$> v .: "id"
  parseJSON v = failExpectObj v
