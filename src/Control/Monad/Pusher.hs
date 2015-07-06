module Control.Monad.Pusher where

import Control.Monad.Error (ErrorT)
import Control.Monad.Reader (ReaderT)

import Data.Pusher (Pusher)

type PusherM a = ReaderT Pusher (ErrorT String IO) a
