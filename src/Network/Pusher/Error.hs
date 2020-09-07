module Network.Pusher.Error
  ( PusherError (..),
  )
where

import Control.Exception (Exception)
import qualified Data.Text as T
import Data.Word (Word16)

data PusherError
  = -- | Received non 200 response code from Pusher.
    Non200Response
      { non200ResponseStatusCode :: Word16,
        non200ResponseBody :: T.Text
      }
  | -- | Received unexpected data from Pusher.
    InvalidResponse T.Text
  deriving (Show)

instance Exception PusherError
