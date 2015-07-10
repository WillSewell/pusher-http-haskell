{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Pusher (MonadPusher, PusherM, PusherT, runPusherT) where

import Control.Monad.Error (ErrorT, MonadError, runErrorT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)

import Control.Monad.Pusher.HTTP (MonadHTTP)
import Data.Pusher (Pusher)

type PusherM a = PusherT Identity a

type PusherT m a = ReaderT Pusher (ErrorT String m) a

runPusherT :: PusherT m a -> Pusher -> m (Either String a)
runPusherT run p = runErrorT $ runReaderT run p

class
  ( MonadError String m
  , MonadReader Pusher m
  , MonadIO m
  , MonadHTTP m
  , Functor m
  ) => MonadPusher m
