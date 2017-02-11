module Network.Pusher.Internal.Payloads.Apns
  ( Apns(..)
  , ApnsAlert(..)) where

import Data.Aeson ((.:), (.=))
import Data.Default (Default(..))
import qualified Data.Aeson as A
import qualified Data.Text as T

import Network.Pusher.Internal.Util (failExpect, failExpectObj)

-- |Documentation available <https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/TheNotificationPayload.html here>.
data Apns = Apns
  { apnsAlert :: ApnsAlert
  , apnsBadge :: Maybe Int
  , apnsSound :: T.Text
  , apnsContentAvailable :: Bool
  , apnsCategory :: T.Text
  , apnsThreadId :: T.Text
  } deriving (Eq, Show)

instance A.ToJSON Apns where
  toJSON apns = A.object
    [ "alert" .= apnsAlert apns
    , "badge" .= apnsBadge apns
    , "sound" .= apnsSound apns
    , "content-available" .= if apnsContentAvailable apns then (1 :: Int) else 0
    , "category" .= apnsCategory apns
    , "thread-id" .= apnsThreadId apns
    ]

instance A.FromJSON Apns where
  parseJSON (A.Object v) =
    Apns
      <$> v .: "alert"
      <*> v .: "badge"
      <*> v .: "sound"
      <*> v .: "content-available"
      <*> v .: "category"
      <*> v .: "thread-id"
  parseJSON v = failExpectObj v

instance Default Apns where
  def = Apns
    { apnsAlert = def
    , apnsBadge = Nothing
    , apnsSound = ""
    , apnsContentAvailable = False
    , apnsCategory = ""
    , apnsThreadId = ""
    }

data ApnsAlert =
  ApnsAlertText T.Text |
  ApnsAlertDict
    { apnsAlertDictTitle :: T.Text
    , apnsAlertDictBody :: T.Text
    , apnsAlertDictTitleLocKey :: Maybe T.Text
    , apnsAlertDictTitleLocArgs :: Maybe [T.Text]
    , apnsAlertDictActionLocKey :: Maybe T.Text
    , apnsAlertDictLocKey :: T.Text
    , apnsAlertDictLocArgs :: [T.Text]
    , apnsAlertDictLaunchImage :: T.Text
    }
  deriving (Eq, Show)

instance A.ToJSON ApnsAlert where
  toJSON (ApnsAlertText a) = A.toJSON a
  toJSON apnsAlertDict = A.object
    [ "title" .= apnsAlertDictTitle apnsAlertDict
    , "body" .= apnsAlertDictBody apnsAlertDict
    , "title-loc-key" .= apnsAlertDictTitleLocKey apnsAlertDict
    , "title-loc-args" .= apnsAlertDictTitleLocArgs apnsAlertDict
    , "action-loc-key" .= apnsAlertDictActionLocKey apnsAlertDict
    , "loc-key" .= apnsAlertDictLocKey apnsAlertDict
    , "loc-args" .= apnsAlertDictLocArgs apnsAlertDict
    , "launch-image" .= apnsAlertDictLaunchImage apnsAlertDict
    ]

instance A.FromJSON ApnsAlert where
  parseJSON (A.String v) = return $ ApnsAlertText v
  parseJSON (A.Object v) =
    ApnsAlertDict
      <$> v .: "title"
      <*> v .: "body"
      <*> v .: "title-loc-key"
      <*> v .: "title-loc-args"
      <*> v .: "action-loc-key"
      <*> v .: "loc-key"
      <*> v .: "loc-args"
      <*> v .: "launch-image"
  parseJSON v = failExpect "JSON object or string" v

instance Default ApnsAlert where
  def = ApnsAlertText "Default Alert"
