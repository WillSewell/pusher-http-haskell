{-# LANGUAGE RecordWildCards #-}

module Network.Pusher.Internal.Payloads.Gcm
  ( Gcm(..)
  , GcmNotification ) where

import Data.Aeson ((.:), (.:?), (.=))
import Data.Default (Default(..))
import qualified Data.Aeson as A
import qualified Data.Text as T

import Network.Pusher.Internal.Util (failExpect, failExpectObj)

-- |Documentation available <https://developers.google.com/cloud-messaging/http-server-ref here>.
data Gcm = Gcm
  -- | Targets
  { to :: T.Text
  , registrationIds :: [T.Text]
  -- | Options
  , collapseKey :: Maybe T.Text
  , priority :: Maybe GcmPriority
  , contentAvailable :: Maybe Bool
  , delayWhileIdle :: Maybe Bool
  , timeToLive :: Maybe Int
  , restrictedPackageName :: Maybe T.Text
  , dryRun :: Maybe Bool
  -- | Payload
  , customData :: Maybe A.Object
  , notification :: Maybe GcmNotification
  }
  deriving (Eq, Show)

--TODO
instance A.ToJSON Gcm where
  toJSON Gcm {..} = A.object
    [ "to" .= to
    , "registration_ids" .= registrationIds
    , "collapse_key" .= collapseKey
    , "priority" .= priority
    , "content_available" .= contentAvailable
    , "delay_while_idle" .= delayWhileIdle
    , "time_to_live" .= timeToLive
    , "restricted_package_name" .= restrictedPackageName
    , "dry_run" .= dryRun
    , "data" .= customData
    , "notification" .= notification
    ]

instance A.FromJSON Gcm where
  parseJSON (A.Object v) =
    Gcm
      <$> v .: "to"
      <*> v .: "registration_ids"
      <*> v .:? "collapse_key"
      <*> v .:? "priority"
      <*> v .:? "content_available"
      <*> v .:? "delay_while_idle"
      <*> v .:? "time_to_live"
      <*> v .:? "restricted_package_name"
      <*> v .:? "dry_run"
      <*> v .:? "data"
      <*> v .:? "notification"
  parseJSON v = failExpectObj v

instance Default Gcm where
  def = Gcm
    { to = ""
    , registrationIds = [""]
    , collapseKey = Nothing
    , priority = Nothing
    , contentAvailable = Nothing
    , delayWhileIdle = Nothing
    , timeToLive = Nothing
    , restrictedPackageName = Nothing
    , dryRun = Nothing
    , customData = Nothing
    , notification = Nothing
    }

data GcmPriority = Normal | High deriving (Eq)

instance Show GcmPriority where
  show Normal = "normal"
  show High = "high"

instance A.ToJSON GcmPriority where
  toJSON prio = A.toJSON . show $ prio

instance A.FromJSON GcmPriority where
  parseJSON (A.String "normal") = pure Normal
  parseJSON (A.String "high") = pure High
  parseJSON v = failExpect "JSON string" v

data GcmNotification = GcmNotification
  { title :: T.Text
  , body :: Maybe T.Text
  , icon :: T.Text
  , sound :: Maybe T.Text
  , badge :: Maybe T.Text
  , tag :: Maybe T.Text
  , color :: Maybe T.Text
  , clickAction :: Maybe T.Text
  , bodyLocKey :: Maybe T.Text
  , bodyLocArgs :: Maybe [T.Text]
  , titleLocKey :: Maybe T.Text
  , titleLocArgs :: Maybe [T.Text]
  } deriving (Eq, Show)

instance A.ToJSON GcmNotification where
  toJSON (GcmNotification {..}) = A.object
    [ "title" .= title
    , "body" .= body
    , "icon" .= icon
    , "sound" .= sound
    , "badge" .= badge
    , "tag" .= tag
    , "color" .= color
    , "click_action" .= clickAction
    , "body_loc_key" .= bodyLocKey
    , "body_loc_args" .= bodyLocArgs
    , "title_loc_key" .= titleLocKey
    , "title_loc_args" .= titleLocArgs
    ]

instance A.FromJSON GcmNotification where
  parseJSON (A.Object v) =
    GcmNotification
      <$> v .: "title"
      <*> v .:? "body"
      <*> v .: "icon"
      <*> v .:? "sound"
      <*> v .:? "badge"
      <*> v .:? "tag"
      <*> v .:? "color"
      <*> v .:? "click_action"
      <*> v .:? "body_loc_key"
      <*> v .:? "body_loc_args"
      <*> v .:? "title_loc_key"
      <*> v .:? "title_loc_args"
  parseJSON v = failExpectObj v
