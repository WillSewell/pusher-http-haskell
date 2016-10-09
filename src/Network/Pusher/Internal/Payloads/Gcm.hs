{-# LANGUAGE RecordWildCards #-}

module Network.Pusher.Internal.Payloads.Gcm
  -- ( createGCMPayload ) where
  ( GcmPayload
  , GcmNotification ) where

import Data.Aeson ((.=))
import Data.Default (Default(..))
import qualified Data.Aeson as A
import qualified Data.Text as T

-- |Documentation available <https://developers.google.com/cloud-messaging/http-server-ref here>.
data GcmPayload = GcmPayload
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
instance A.ToJSON GcmPayload where
  toJSON GcmPayload {..} = A.object
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

instance Default GcmPayload where
  def = GcmPayload
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
