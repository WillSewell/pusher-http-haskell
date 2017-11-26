module Main where

import Control.Arrow
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.CaseInsensitive (original)
import qualified Data.Yaml as Y
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap

import qualified Network.Pusher as P

main :: IO ()
main = do
  eitherCred <- Y.decodeFileEither "../credentials.yaml"
  case eitherCred of
    Left e -> print e
    Right cred -> do
      pusher <- P.getPusher cred
      Snap.quickHttpServe $ Snap.method Snap.POST $ webhookHandler pusher

webhookHandler :: P.Pusher -> Snap.Snap ()
webhookHandler pusher = do
  req <- Snap.getRequest
  body <- Snap.readRequestBody 2048
  let headers :: [(BS.ByteString, BS.ByteString)]
      headers = map (first original) . Snap.listHeaders . Snap.rqHeaders $ req
      mWebhookPayload = P.parseWebhookPayload pusher headers (BL.toStrict body)
  case mWebhookPayload of
    Nothing -> Snap.modifyResponse $ Snap.setResponseStatus 403 "FORBIDDEN"
    Just (P.WebhookPayload _key _verifiedSignature (P.Webhooks time webhookEvs)) -> do
      liftIO $ mapM_ (webhookF time) webhookEvs
      Snap.modifyResponse $ Snap.setResponseStatus 200 "OK"
  where
    webhookF _utcTime ev =
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
          , " on channel "
          , show c
          ]
