{-|
Module      : Control.Monad.Pusher.HTTP
Description : Typclass for IO monads that can issue HTTP requests
Copyright   : (c) Will Sewell, 2015
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

A typeclass for IO monads that can issue get and post requests. The intention is
to use the IO instance in the main library, and a mock in the tests.
-}
module Control.Monad.Pusher.HTTP (MonadHTTP(..)) where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Client (Manager, Request, Response)
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Client as HTTP.Client

-- |These functions essentially abstract the corresponding functions from the
-- Wreq library.
class Monad m => MonadHTTP m where
  httpLbs :: Request -> Manager -> m (Response BL.ByteString)

instance MonadHTTP IO where
  httpLbs = HTTP.Client.httpLbs

instance MonadHTTP m => MonadHTTP (ReaderT r m) where
  httpLbs req mngr = lift $ httpLbs req mngr

instance MonadHTTP m => MonadHTTP (ExceptT e m) where
  httpLbs req mngr = lift $ httpLbs req mngr
