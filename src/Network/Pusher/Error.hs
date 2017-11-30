module Network.Pusher.Error
  ( PusherError(..)
  ) where

import Control.Exception (Exception)
import qualified Data.Text as T

data PusherError
  = PusherArgumentError T.Text
  -- ^Data from the caller is not valid.
  | PusherNon200ResponseError T.Text
  -- ^Received non 200 response code from Pusher.
  | PusherInvalidResponseError T.Text
  -- ^Received unexpected data from Pusher.
  deriving (Show)

instance Exception PusherError
