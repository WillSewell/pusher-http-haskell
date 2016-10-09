{-# LANGUAGE DeriveGeneric #-}

module Network.Pusher.Internal.Payloads.Fcm

-- ()

where

import Data.Default (Default(..))
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Text as T

data FcmPayload = FcmPayload T.Text deriving (Eq, Generic, Show)

--TODO
instance A.ToJSON FcmPayload

instance Default FcmPayload where
  def = FcmPayload
    "default Fcm"
