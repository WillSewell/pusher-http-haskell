module Main where

import Control.Monad.Pusher (PusherT, runPusherT)
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
  eitherCred <- Y.decodeFileEither "example/credentials.yaml"
  case eitherCred of
    Right cred -> do
      result <- runPusherT pusherApp (getPusher cred)
      case result of
        Right (channels, channel, users) -> do
          print $ "Channels info: " ++ show channels
          print $ "Channel info: " ++ show channel
          print $ "Userss info: " ++ show users
        Left e -> print e
    Left e -> print e

pusherApp :: PusherT IO (P.ChannelsInfo, P.ChannelInfo, P.Users)
pusherApp = do
  P.trigger ["presence-messages"] "some_event" "data" Nothing
  channels <- P.channels "presence-" (P.ChannelsInfoQuery (HS.singleton P.ChannelsUserCount))
  channel <- P.channel "presence-messages" (P.ChannelInfoQuery (HS.singleton P.ChannelUserCount))
  users <- P.users "presence-messages"
  return (channels, channel, users)

