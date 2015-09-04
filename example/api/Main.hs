module Main where

import Control.Monad.Pusher (PusherT, runPusherT)
import qualified Data.HashSet as HS
import qualified Data.Pusher as P
import qualified Data.Yaml as Y
import qualified Network.Pusher as P
import qualified Network.Pusher.Protocol as P

main :: IO ()
main = do
  eitherCred <- Y.decodeFileEither "example/credentials.yaml"
  case eitherCred of
    Right cred -> do
      pusher <- P.getPusher cred
      result <- runPusherT pusherApp pusher
      case result of
        Right (channels, channel, users) -> do
          print $ "Channels info: " ++ show channels
          print $ "Channel info: " ++ show channel
          print $ "Userss info: " ++ show users
        Left e -> print e
    Left e -> print e

pusherApp :: PusherT IO (P.ChannelsInfo, P.FullChannelInfo, P.Users)
pusherApp = do
  let chan = P.Channel P.Presence "messages"
  P.trigger [chan] "some_event" "data" Nothing
  channels <- P.channels
    (Just P.Presence)
    ""
    (P.ChannelsInfoQuery (HS.singleton P.ChannelsUserCount))
  channel <- P.channel
    chan
    (P.ChannelInfoQuery
      (HS.fromList [P.ChannelUserCount, P.ChannelSubscriptionCount]))
  users <- P.users chan
  return (channels, channel, users)
