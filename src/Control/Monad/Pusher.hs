module Control.Monad.Pusher where

import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT, runReaderT)

import Data.Pusher (Pusher)

type PusherM a = PusherT Identity a

type PusherT m a = ReaderT Pusher (ErrorT String m) a

runPusherT :: PusherT m a -> Pusher -> m (Either String a)
runPusherT run p = runErrorT $ runReaderT run p
