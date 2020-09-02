-- |
-- Module      : Network.Pusher.Internal.Util
-- Description : Various utility functions
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : experimental
module Network.Pusher.Internal.Util
  ( failExpectObj,
    failExpectStr,
    show',
    getSystemTimeSeconds,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.String (IsString, fromString)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime)
import Data.Word (Word64)

-- | When decoding Aeson values, this can be used to return a failing parser
--  when an object was expected, but it was a different type of data.
failExpectObj :: A.Value -> A.Parser a
failExpectObj = fail . ("Expected JSON object, got " ++) . show

-- | When decoding Aeson values, this can be used to return a failing parser
--  when an string was expected, but it was a different type of data.
failExpectStr :: A.Value -> A.Parser a
failExpectStr = fail . ("Expected JSON string, got " ++) . show

-- | Generalised version of show.
show' :: (Show a, IsString b) => a -> b
show' = fromString . show

getSystemTimeSeconds :: MonadIO m => m Word64
getSystemTimeSeconds = do
  t <- liftIO getSystemTime
  return $ fromIntegral $ systemSeconds $ t
