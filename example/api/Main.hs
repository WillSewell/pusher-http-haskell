module Main where

import Control.Exception (displayException)
import Data.Monoid ((<>))
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import qualified Network.Pusher as P
import qualified Network.Pusher.Protocol as P

main :: IO ()
main = do
  eitherCred <- Y.decodeFileEither "../credentials.yaml"
  case eitherCred of
    Left e -> print e
    Right cred -> do
      pusher <- P.getPusher cred
      demoTrigger pusher
      demoChannels pusher
      demoChannel pusher
      demoUsers pusher

channel :: P.Channel
channel = P.Channel P.Presence "messages"

demoTrigger :: P.Pusher -> IO ()
demoTrigger pusher = do
  triggerResult <- P.trigger pusher [channel] "some_event" "data" Nothing
  case triggerResult of
    Left e -> T.putStrLn $ "trigger failed: " <> T.pack (displayException e)
    Right () -> return ()

demoChannels :: P.Pusher -> IO ()
demoChannels pusher = do
  let channelsInfoQuery = P.ChannelsInfoQuery $ HS.singleton P.ChannelsUserCount
  channelsResult <- P.channels pusher (Just P.Presence) "" channelsInfoQuery
  case channelsResult of
    Left e -> T.putStrLn $ "channels failed: " <> T.pack (displayException e)
    Right channels -> putStrLn $ "channels result: " ++ show channels

demoChannel :: P.Pusher -> IO ()
demoChannel pusher = do
  let
    chanAttrs = HS.fromList [P.ChannelUserCount, P.ChannelSubscriptionCount]
    channelInfoQuery = P.ChannelInfoQuery chanAttrs
  channelResult <- P.channel pusher channel channelInfoQuery
  case channelResult of
    Left e -> T.putStrLn $ "channel failed: " <> T.pack (displayException e)
    Right chan -> putStrLn $ "channel result: " ++ show chan

demoUsers :: P.Pusher -> IO ()
demoUsers pusher = do
  usersResult <- P.users pusher channel
  case usersResult of
    Left e -> T.putStrLn $ "users failed: " <> T.pack (displayException e)
    Right users -> putStrLn $ "users result: " ++ show users
