module Main where

import qualified Data.Yaml as Y
import qualified Network.Pusher as P

main :: IO ()
main = do
  eitherCred <- Y.decodeFileEither "../credentials.yaml"
  case eitherCred of
    Left e -> print e
    Right cred -> do
      pusher <- P.getPusher cred
      echoWebhooks pusher
      putStrLn "Waiting for newline"
      -- Wait for a newline to terminate the program
      _ <- getLine
      return ()

echoWebhooks :: P.Pusher -> IO ()
echoWebhooks pusher = do
  _threadId <-
    P.handleWebhooks pusher $ \_utcTime ev ->
      putStrLn . mconcat $
      case ev of
        P.ChannelOccupiedEv c -> ["channel ", show c, " is now occupied."]
        P.ChannelVacatedEv c -> ["channel ", show c, " is now vacated."]
        P.MemberAddedEv c user ->
          ["user ", show user, " joined presence channel ", show c]
        P.MemberRemovedEv c user ->
          ["user ", show user, " left presence channel ", show c]
        P.ClientEv c evName evBody sockID mUser ->
          [ maybe "an unknown user " (\u -> "user " ++ show u) mUser
          , " with socket id "
          , show sockID
          , " sent a client event named "
          , show evName
          , " with data "
          , show evBody
          ]
  return ()
