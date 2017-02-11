{-# LANGUAGE DeriveGeneric #-}

module Network.Pusher.Internal.Payload.Fcm

-- ()

where

import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Text as T

data Fcm = Fcm T.Text deriving (Eq, Generic, Show)

--TODO
instance A.ToJSON Fcm

instance A.FromJSON Fcm
