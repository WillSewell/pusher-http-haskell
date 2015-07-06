module Main where

import Control.Monad.Pusher (runPusherT)
import Data.Monoid ((<>))
import qualified Data.HashSet as HS
import qualified Data.Pusher as P
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Pusher as P
import qualified Pusher.Protocol as P

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
--       x <- runPusherT
--           (P.channel
--             "test_channel"
--             (P.ChannelInfoQuery (HS.singleton P.ChannelUserCount)))
--           pusher
      x <- runPusherT
          (P.users
            "presence-test")
          pusher
      case x of
        Right r -> print r
        Left e -> print e
    Left e -> print e
