{-# LANGUAGE CPP #-}

module Main where

import Control.Exception (displayException)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import qualified Network.Pusher as P

main :: IO ()
main = do
  eitherCred <- Y.decodeFileEither "../credentials.yaml"
  case eitherCred of
    Left e -> print e
    Right cred -> do
      pusher <- P.getPusher cred
      demoPushNotification pusher

demoPushNotification :: P.Pusher -> IO ()
demoPushNotification pusher = do
  let Just interest = P.mkInterest "exampleinterest"
      fcmObject :: A.Object
      fcmObject =
        H.fromList
          [ ( "notification"
            , A.Object $
              H.fromList
                [ ("title", A.String "Sup?")
                , ("body", A.String "Hello Firebase.")
                , ("icon", A.String "firebase-logo.png")
                ])
          ]
      notification =
        P.Notification
        { P.notificationInterest = interest
        , P.notificationWebhookURL = Nothing
        , P.notificationWebhookLevel = Nothing
        , P.notificationAPNSPayload = Nothing
        , P.notificationGCMPayload = Nothing
        , P.notificationFCMPayload = Just $ P.FCMPayload fcmObject
        }
  notifyResult <- P.notify pusher notification
  T.putStrLn $
    case notifyResult of
      Left e -> "Notify failed: " <> T.pack (displayException e)
      Right () -> "Notify success"
