module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y
import qualified Network.URL as URL
import qualified Network.HTTP.Server as HTTP
import qualified Network.Socket.Internal as Sock
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
echoWebhooks pusher = HTTP.serverWith config handler
  where
    config = HTTP.defaultConfig {HTTP.srvPort = 80}

    handler :: Sock.SockAddr -> URL.URL -> HTTP.Request B.ByteString -> IO (HTTP.Response B.ByteString)
    handler _ _url req =
      let headers         = map (\(HTTP.Header k v) -> (B.pack . show $ k, B.pack . show $ v)) . HTTP.rqHeaders $ req
          body            = HTTP.rqBody req
          mWebhookPayload = P.parseWebhookPayload pusher headers body
         in case mWebhookPayload of
              Nothing
                -> return . HTTP.respond $ HTTP.Forbidden

              Just (P.WebhookPayload _key _verifiedSignature (P.Webhooks time webhookEvs))
                -> do mapM_ (webhookF time) webhookEvs
                      return . HTTP.respond $ HTTP.OK

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

