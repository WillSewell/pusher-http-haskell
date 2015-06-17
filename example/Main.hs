module Main where

import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.Reader (ReaderT, runReaderT)

import qualified Data.HashSet as HS
import qualified Pusher as P

type PusherM a = ReaderT P.Pusher (ErrorT String IO) a

pusher :: P.Pusher
pusher = P.Pusher
  { P.pusher'endpoint = "http://api.pusherapp.com/apps/100462/"
  , P.pusher'credentials = P.Credentials
    { P.credentials'appID = 100462
    , P.credentials'appKey = "7435d7e5de69a077ebe0"
    , P.credentials'appSecret = "8b8400f3d5c70ae238ac"
    }
  }

main :: IO ()
main = do
--  x <- runErrorT (runReaderT (P.trigger ["c"] "e" "d" Nothing) pusher)
  x <- runErrorT (runReaderT (P.channels "" (P.ChannelInfoQuery (HS.singleton P.UserCount))) pusher)
  case x of
    Right r -> print r
    Left e -> print e
