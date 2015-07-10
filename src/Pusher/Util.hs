{-|
Module      : Pusher.Util
Description : Various utilty functions
Copyright   : (c) Will Sewell, 2015
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental
-}
module Pusher.Util
  ( failExpectObj
  , getIntPOSIXTime
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

-- |When decoding Aeson values, this can be used to return a failing parser
-- when an object was expected, but it was a different type of data.
failExpectObj :: A.Value -> A.Parser a
failExpectObj = fail . ("Expected JSON object, got " ++) . show

-- | Wrapper around getPOSIXTime that rounds the time to an Int.
getIntPOSIXTime :: MonadIO m => m Int
getIntPOSIXTime =  liftIO $ round <$> getPOSIXTime
