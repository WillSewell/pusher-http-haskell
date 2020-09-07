module Network.Pusher.Error
  ( PusherError (..),
  )
where

import Control.Exception (Exception)
import qualified Data.Text as T

data PusherError
  = -- | Received non 200 response code from Pusher.
    Non200Response T.Text
  | -- | Received unexpected data from Pusher.
    InvalidResponse T.Text
  deriving (Show)

instance Exception PusherError
