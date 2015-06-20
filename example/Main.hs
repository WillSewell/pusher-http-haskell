module Main where

import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.Reader (ReaderT, runReaderT)

import qualified Data.HashSet as HS
import qualified Data.Yaml as Y
import qualified Pusher as P

type PusherM a = ReaderT P.Pusher (ErrorT String IO) a

getPusher :: P.Credentials -> P.Pusher
getPusher cred = P.Pusher
  { P.pusher'endpoint = "http://api.pusherapp.com/apps/100462/"
  , P.pusher'credentials = cred
  }

main :: IO ()
main = do
  maybeCred <- Y.decodeFile "credentials.yaml"
  case maybeCred of
    Just cred -> do
      let pusher = getPusher cred
      x <- runErrorT (runReaderT (P.trigger ["c"] "e" "d" Nothing) pusher)
      case x of
        Right r -> print r
        Left e -> print e
    Nothing -> print ("Failed to decode credentials" :: String)
