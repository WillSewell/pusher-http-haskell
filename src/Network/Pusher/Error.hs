module Network.Pusher.Error
  ( PusherError (..),
  )
where

import Control.Exception (Exception)
import qualified Data.Text as T

data PusherError
  = -- | Received non 200 response code from Pusher.
    PusherNon200ResponseError T.Text
  | -- | Received unexpected data from Pusher.
    PusherInvalidResponseError T.Text
  deriving (Show)

instance Exception PusherError
