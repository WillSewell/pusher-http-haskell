{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pusher.Protocol
  ( ChannelInfo(..)
  , ChannelInfoAttributes(..)
  , ChannelInfoQuery(..)
  , ChannelsInfo(..)
  , ChannelsInfoQuery(..)
  , ChannelsInfoAttributes(..)
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

import Pusher.Util (failExpectObj)

class ToURLParam a where
  toURLParam :: a -> T.Text

data ChannelsInfoAttributes = ChannelsUserCount deriving Generic

instance ToURLParam ChannelsInfoAttributes where
  toURLParam ChannelsUserCount = "user_count"

instance Hashable ChannelsInfoAttributes

newtype ChannelsInfoQuery =
  ChannelsInfoQuery (HS.HashSet ChannelsInfoAttributes)
  deriving ToURLParam


data ChannelInfoAttributes = ChannelUserCount | ChannelSubscriptionCount
  deriving Generic

instance ToURLParam ChannelInfoAttributes where
  toURLParam ChannelUserCount = "user_count"
  toURLParam ChannelSubscriptionCount = "subscription_count"

instance Hashable ChannelInfoAttributes

newtype ChannelInfoQuery = ChannelInfoQuery (HS.HashSet ChannelInfoAttributes)
  deriving ToURLParam


instance ToURLParam a => ToURLParam (HS.HashSet a) where
  toURLParam hs = T.concat $ toURLParam <$> HS.toList hs


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


data ChannelInfoAttributeResp = UserCountResp Int deriving Show

instance Hashable ChannelInfoAttributeResp where
  hashWithSalt salt (UserCountResp count) = hashWithSalt salt count
