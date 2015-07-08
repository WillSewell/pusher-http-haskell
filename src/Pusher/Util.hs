module Pusher.Util
  ( failExpectObj
  , getIntPOSIXTime
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

failExpectObj :: A.Value -> A.Parser a
failExpectObj = fail . ("Expected JSON object, got " ++) . show

getIntPOSIXTime :: MonadIO m => m Int
getIntPOSIXTime =  liftIO $ round <$> getPOSIXTime
