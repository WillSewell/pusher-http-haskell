-- |
-- Module      : Network.Pusher.Internal.Util
-- Description : Various utility functions
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : experimental
module Network.Pusher.Internal.Util
  ( show',
    getSystemTimeSeconds,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (IsString, fromString)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime)
import Data.Word (Word64)

-- | Generalised version of show.
show' :: (Show a, IsString b) => a -> b
show' = fromString . show

getSystemTimeSeconds :: MonadIO m => m Word64
getSystemTimeSeconds = do
  t <- liftIO getSystemTime
  return $ fromIntegral $ systemSeconds t
