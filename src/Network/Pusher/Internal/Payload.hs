{-# LANGUAGE CPP #-}

module Network.Pusher.Internal.Payload
  ( Payload
  , Payloadource(..)
  , Apns, Gcm, Fcm
  , payloadObject
  ) where

#if MIN_VERSION_base(4,9,0)
import Data.Monoid(Monoid(..))
import Data.Semigroup(Semigroup(..))
#else
import Data.Monoid((<>))
#endif

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

import Network.Pusher.Internal.Payload.Apns
import Network.Pusher.Internal.Payload.Gcm
import Network.Pusher.Internal.Payload.Fcm

newtype Payload = Payload A.Object deriving (Eq, Show)

#if MIN_VERSION_base(4,9,0)

instance Semigroup Payload where
  Payload a <> Payload b = Payload (a <> b)

instance Monoid Payload where
  mappend = (<>)
  mempty = Payload HM.empty

#else

instance Monoid Payload where
  Payload a `mappend` Payload b = Payload (a `mappend` b)
  mempty = Payload HM.empty

#endif

class Payloadource a where
  renderPayload :: a -> Payload

instance Payloadource Apns where
  renderPayload a = Payload $ HM.singleton "apns" (A.toJSON a)

instance Payloadource Gcm where
  renderPayload a = Payload $ HM.singleton "gcm" (A.toJSON a)

instance Payloadource Fcm where
  renderPayload a = Payload $ HM.singleton "fcm" (A.toJSON a)

combinePayload :: Payloadource a => a -> Payload -> Payload
combinePayload source payload =
  renderPayload source <> payload

payloadObject :: Payload -> A.Object
payloadObject (Payload payload) = payload
