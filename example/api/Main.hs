module Main where

import Control.Exception (displayException)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import qualified Network.Pusher as P
import qualified Network.Pusher.Protocol as P

main :: IO ()
main = do
  eitherSettings <- Y.decodeFileEither "../settings.yaml"
  case eitherSettings of
    Left e -> print e
    Right settings -> do
      pusher <- P.newPusher settings
      demoTrigger pusher
      demoTriggerBatch pusher
      demoChannels pusher
      demoChannel pusher
      demoUsers pusher

demoTrigger :: P.Pusher -> IO ()
demoTrigger pusher = do
  triggerResult <- P.trigger pusher ["presence-messages"] "some_event" "data" Nothing
  case triggerResult of
    Left e -> T.putStrLn $ "trigger failed: " <> T.pack (displayException e)
    Right () -> return ()

demoTriggerBatch :: P.Pusher -> IO ()
demoTriggerBatch pusher = do
  let events =
        [ P.Event "presence-messages" "some_event_0" "data_0" Nothing,
          P.Event "other-messages" "some_event_1" "data_1" Nothing
        ]
  triggerResult <- P.triggerBatch pusher events
  case triggerResult of
    Left e -> T.putStrLn $ "triggerBatch failed: " <> T.pack (displayException e)
    Right () -> return ()

demoChannels :: P.Pusher -> IO ()
demoChannels pusher = do
  let channelsInfoQuery = P.ChannelsInfoQuery $ HS.singleton P.ChannelsUserCount
  channelsResult <- P.channels pusher "presence-" channelsInfoQuery
  case channelsResult of
    Left e -> T.putStrLn $ "channels failed: " <> T.pack (displayException e)
    Right channels -> putStrLn $ "channels result: " ++ show channels

demoChannel :: P.Pusher -> IO ()
demoChannel pusher = do
  let chanAttrs = HS.fromList [P.ChannelUserCount, P.ChannelSubscriptionCount]
      channelInfoQuery = P.ChannelInfoQuery chanAttrs
  channelResult <- P.channel pusher "presence-messages" channelInfoQuery
  case channelResult of
    Left e -> T.putStrLn $ "channel failed: " <> T.pack (displayException e)
    Right chan -> putStrLn $ "channel result: " ++ show chan

demoUsers :: P.Pusher -> IO ()
demoUsers pusher = do
  usersResult <- P.users pusher "presence-messages"
  case usersResult of
    Left e -> T.putStrLn $ "users failed: " <> T.pack (displayException e)
    Right users -> putStrLn $ "users result: " ++ show users
