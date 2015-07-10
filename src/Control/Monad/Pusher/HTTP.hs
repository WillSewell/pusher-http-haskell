{-
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

import Control.Monad.Error (Error, ErrorT)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq as W
import qualified Network.Wreq.Types as WT

-- |These functions essentially abstract the corresponding functions from the
-- Wreq library.
class Monad m => MonadHTTP m where
  getWith :: W.Options -> String -> m (W.Response BL.ByteString)
  postWith :: WT.Postable a => W.Options -> String -> a -> m (W.Response BL.ByteString)

instance MonadHTTP IO where
  getWith = W.getWith
  postWith = W.postWith

instance MonadHTTP m => MonadHTTP (ReaderT r m) where
  getWith = getWith
  postWith = postWith

instance (Error e, MonadHTTP m) => MonadHTTP (ErrorT e m) where
  getWith = getWith
  postWith = postWith
