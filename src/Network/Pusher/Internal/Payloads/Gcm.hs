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
  { gcmTo :: T.Text
  , gcmRegistrationIds :: [T.Text]
  -- | Options
  , gcmCollapseKey :: Maybe T.Text
  , gcmPriority :: Maybe GcmPriority
  , gcmContentAvailable :: Maybe Bool
  , gcmDelayWhileIdle :: Maybe Bool
  , gcmTimeToLive :: Maybe Int
  , gcmRestrictedPackageName :: Maybe T.Text
  , gcmDryRun :: Maybe Bool
  -- | Payload
  , gcmCustomData :: Maybe A.Object
  , gcmNotification :: Maybe GcmNotification
  }
  deriving (Eq, Show)

--TODO
instance A.ToJSON Gcm where
  toJSON gcm = A.object
    [ "to" .= gcmTo gcm
    , "registration_ids" .= gcmRegistrationIds gcm
    , "collapse_key" .= gcmCollapseKey gcm
    , "priority" .= gcmPriority gcm
    , "content_available" .= gcmContentAvailable gcm
    , "delay_while_idle" .= gcmDelayWhileIdle gcm
    , "time_to_live" .= gcmTimeToLive gcm
    , "restricted_package_name" .= gcmRestrictedPackageName gcm
    , "dry_run" .= gcmDryRun gcm
    , "data" .= gcmCustomData gcm
    , "notification" .= gcmNotification gcm
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
    { gcmTo = ""
    , gcmRegistrationIds = [""]
    , gcmCollapseKey = Nothing
    , gcmPriority = Nothing
    , gcmContentAvailable = Nothing
    , gcmDelayWhileIdle = Nothing
    , gcmTimeToLive = Nothing
    , gcmRestrictedPackageName = Nothing
    , gcmDryRun = Nothing
    , gcmCustomData = Nothing
    , gcmNotification = Nothing
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
  { gcmNotificationTitle :: T.Text
  , gcmNotificationBody :: Maybe T.Text
  , gcmNotificationIcon :: T.Text
  , gcmNotificationSound :: Maybe T.Text
  , gcmNotificationBadge :: Maybe T.Text
  , gcmNotificationTag :: Maybe T.Text
  , gcmNotificationColor :: Maybe T.Text
  , gcmNotificationClickAction :: Maybe T.Text
  , gcmNotificationBodyLocKey :: Maybe T.Text
  , gcmNotificationBodyLocArgs :: Maybe [T.Text]
  , gcmNotificationTitleLocKey :: Maybe T.Text
  , gcmNotificationTitleLocArgs :: Maybe [T.Text]
  } deriving (Eq, Show)

instance A.ToJSON GcmNotification where
  toJSON notification = A.object
    [ "title" .= gcmNotificationTitle notification
    , "body" .= gcmNotificationBody notification
    , "icon" .= gcmNotificationIcon notification
    , "sound" .= gcmNotificationSound notification
    , "badge" .= gcmNotificationBadge notification
    , "tag" .= gcmNotificationTag notification
    , "color" .= gcmNotificationColor notification
    , "click_action" .= gcmNotificationClickAction notification
    , "body_loc_key" .= gcmNotificationBodyLocKey notification
    , "body_loc_args" .= gcmNotificationBodyLocArgs notification
    , "title_loc_key" .= gcmNotificationTitleLocKey notification
    , "title_loc_args" .= gcmNotificationTitleLocArgs notification
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
