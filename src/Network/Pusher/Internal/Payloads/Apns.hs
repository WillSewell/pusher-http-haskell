{-# LANGUAGE RecordWildCards #-}

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
  { alert :: ApnsAlert
  , badge :: Maybe Int
  , sound :: T.Text
  , contentAvailable :: Bool
  , category :: T.Text
  , threadId :: T.Text
  } deriving (Eq, Show)

instance A.ToJSON Apns where
  toJSON (Apns {..}) = A.object
    [ "alert" .= alert
    , "badge" .= badge
    , "sound" .= sound
    , "content-available" .= if contentAvailable then (1 :: Int) else 0
    , "category" .= category
    , "thread-id" .= threadId
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
    { alert = def
    , badge = Nothing
    , sound = ""
    , contentAvailable = False
    , category = ""
    , threadId = ""
    }

data ApnsAlert =
  ApnsAlertText T.Text |
  ApnsAlertDict
    { title :: T.Text
    , body :: T.Text
    , titleLocKey :: Maybe T.Text
    , titleLocArgs :: Maybe [T.Text]
    , actionLocKey :: Maybe T.Text
    , locKey :: T.Text
    , locArgs :: [T.Text]
    , launchImage :: T.Text
    }
  deriving (Eq, Show)

instance A.ToJSON ApnsAlert where
  toJSON (ApnsAlertText a) = A.toJSON a
  toJSON ApnsAlertDict {..} = A.object
    [ "title" .= title
    , "body" .= body
    , "title-loc-key" .= titleLocKey
    , "title-loc-args" .= titleLocArgs
    , "action-loc-key" .= actionLocKey
    , "loc-key" .= locKey
    , "loc-args" .= locArgs
    , "launch-image" .= launchImage
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
