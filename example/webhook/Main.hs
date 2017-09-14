module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y
import qualified Network.HTTP.Server as HTTP
import qualified Network.Pusher as P

import Control.Monad (when)

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
echoWebhooks pusher =
  let config = HTTP.defaultConfig {HTTP.srvPort = 80}
     in HTTP.serverWith config  $ \_ _url req -> do success <- P.handleWebhooks pusher (return req) parseReq webhookF
                                                    if success then return $ HTTP.respond HTTP.OK else undefined
  where
    parseReq :: HTTP.Request B.ByteString -> Maybe ([(B.ByteString,B.ByteString)],B.ByteString)
    parseReq req = Just ([],"")

    webhookF _utcTime ev = putStrLn . mconcat $
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

