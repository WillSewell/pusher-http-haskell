{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Control.Monad.Pusher
Description : Monad type aliases for the return types of the external API functions
Copyright   : (c) Will Sewell, 2015
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

Monad type aliases for the return types of the external API functions.

The API functions return a monad that implements MonadPusher. The user of the
library can use PusherT or PusherM as the concrete return type, or they can use
their own types. They are really just aliases for a stack of ReaderT and ErrorT.
-}
module Control.Monad.Pusher (MonadPusher, PusherM, PusherT, runPusherT) where

import Control.Monad.Error (ErrorT, MonadError, runErrorT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)

import Control.Monad.Pusher.HTTP (MonadHTTP)
import Data.Pusher (Pusher)

-- |Monad that can be used as the return type of the main API functions.
type PusherM a = PusherT Identity a

-- |Use this if you wish to stack this on your own monads.
type PusherT m a = ReaderT Pusher (ErrorT String m) a

-- |Run the monadic Pusher code to extract the result
runPusherT :: PusherT m a -> Pusher -> m (Either String a)
runPusherT run p = runErrorT $ runReaderT run p

-- |Typeclass alias for the return type of the API functions (keeps the
-- signatures less verbose)
class
  ( MonadError String m
  , MonadReader Pusher m
  , MonadIO m
  , MonadHTTP m
  , Functor m
  ) => MonadPusher m
