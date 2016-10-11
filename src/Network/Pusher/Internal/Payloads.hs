{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Pusher.Internal.Payloads
  ( Payload
  , PayloadSource(..)
  , Apns, Gcm, Fcm
  , payloadObject
  ) where

import Data.Default(Default(..))
import Data.Monoid(Monoid(..))
import Data.Semigroup(Semigroup(..))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

import Network.Pusher.Internal.Payloads.Apns
import Network.Pusher.Internal.Payloads.Gcm
import Network.Pusher.Internal.Payloads.Fcm

newtype Payload = Payload A.Object deriving (Eq, Show)

instance Semigroup Payload where
  Payload a <> Payload b = Payload (a <> b)

instance Monoid Payload where
  mappend = (<>)
  mempty = Payload HM.empty

instance Default Payload where
  def = mempty

class PayloadSource a where
  renderPayload :: a -> Payload

instance PayloadSource Apns where
  renderPayload a = Payload $ HM.singleton "apns" (A.toJSON a)

instance PayloadSource Gcm where
  renderPayload a = Payload $ HM.singleton "gcm" (A.toJSON a)

instance PayloadSource Fcm where
  renderPayload a = Payload $ HM.singleton "fcm" (A.toJSON a)

combinePayload :: PayloadSource a => a -> Payload -> Payload
combinePayload source payload =
  renderPayload source <> payload

payloadObject :: Payload -> A.Object
payloadObject (Payload payload) = payload


