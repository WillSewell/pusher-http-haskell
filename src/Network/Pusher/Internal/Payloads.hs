{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Pusher.Internal.Payloads
  ( Payload
  , payloadObject
  ) where

import Data.Aeson ((.=))
import Data.Default(Default(..))
import GHC.Generics(Generic(..))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

import Network.Pusher.Internal.Payloads.Apns
import Network.Pusher.Internal.Payloads.Gcm
import Network.Pusher.Internal.Payloads.Fcm

data Payload = Payload
  { apns :: ApnsPayload
  , gcm :: GcmPayload
  , fcm :: FcmPayload
  } deriving (Eq, Show, Generic)

instance Default Payload where
  def = Payload
    { apns = def
    , gcm = def
    , fcm = def
    }

payloadObject :: Payload -> A.Object
payloadObject Payload {..} =
  HM.fromList
    [ "apns" .= apns
    , "gcm" .= gcm
    , "fcm" .= fcm
    ]
