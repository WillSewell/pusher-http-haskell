module Main where

import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Monoid ((<>))
import qualified Data.HashSet as HS
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Pusher as P
import qualified Pusher.Protocol as P

type PusherM a = ReaderT P.Pusher (ErrorT String IO) a

getPusher :: P.Credentials -> P.Pusher
getPusher cred =
  let path = "/apps/" <> T.pack (show $ P.credentials'appID cred) <> "/" in
  P.Pusher
    { P.pusher'host = "http://api.pusherapp.com"
    , P.pusher'path = path
    , P.pusher'credentials = cred
    }

main :: IO ()
main = do
-- x <- runErrorT (runReaderT (P.trigger ["c"] "e" "d" Nothing) pusher)
-- x <- runErrorT (runReaderT (P.channels "presence-" (P.ChannelInfoQuery (HS.singleton P.UserCount))) pusher)
  eitherCred <- Y.decodeFileEither "example/credentials.yaml"
  case eitherCred of
    Right cred -> do
      let pusher = getPusher cred
      x <- runErrorT $
        runReaderT
          (P.channel
            "test_channel"
            (P.ChannelInfoQuery (HS.singleton P.ChannelUserCount)))
          pusher
      case x of
        Right r -> print r
        Left e -> print e
    Left e -> print e
