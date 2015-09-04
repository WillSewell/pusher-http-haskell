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
import Control.Monad.Error (Error, ErrorT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Client (Manager, Request, Response)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Time.Clock.POSIX as Clock
import qualified Network.HTTP.Client as HTTP.Client

class Monad m => MonadTime m where
  getPOSIXTime :: m Int

instance MonadTime IO where
  getPOSIXTime = round <$> Clock.getPOSIXTime

instance MonadTime m => MonadTime (ReaderT r m) where
  getPOSIXTime = lift getPOSIXTime

instance (Error e, MonadTime m) => MonadTime (ErrorT e m) where
  getPOSIXTime = lift getPOSIXTime
