{-# LANGUAGE DeriveGeneric #-}

module Network.Pusher.Internal.Payloads.Fcm

-- ()

where

import Data.Default (Default(..))
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Text as T

data Fcm = Fcm T.Text deriving (Eq, Generic, Show)

--TODO
instance A.ToJSON Fcm

instance A.FromJSON Fcm

instance Default Fcm where
  def = Fcm
    "default Fcm"
