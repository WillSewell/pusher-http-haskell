{-|
Module      : Control.Monad.Pusher.Time
Description : Typclass for IO monads that can get the current time
Copyright   : (c) Will Sewell, 2015
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental
-}
module Control.Monad.Pusher.Time (MonadTime(..)) where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Time.Clock.POSIX as Clock

class Monad m => MonadTime m where
  getPOSIXTime :: m Int

instance MonadTime IO where
  getPOSIXTime = round <$> Clock.getPOSIXTime

instance MonadTime m => MonadTime (ReaderT r m) where
  getPOSIXTime = lift getPOSIXTime

instance MonadTime m => MonadTime (ExceptT e m) where
  getPOSIXTime = lift getPOSIXTime
