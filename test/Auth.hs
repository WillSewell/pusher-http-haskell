module Auth where

import Control.Monad.Reader (runReader)
import Test.Hspec (describe, it, shouldBe)

import Data.Pusher (Credentials(..), Pusher(..))
import Pusher.Auth
  ( authenticatePrivate
  , authenticatePresenceWithEncoder
  , makeQS
  )

test = do
  describe "Auth.makeQS" $
    it "works" $
      -- Happy case based on data from the docs: https://pusher.com/docs/rest_api#worked-authentication-example
      let
        body = "{\"name\":\"foo\",\"channels\":[\"project-3\"],\"data\":\"{\\\"some\\\":\\\"data\\\"}\"}"
      in
        runReader (makeQS "POST" "/apps/3/events" [] body 1353088179) pusher
        `shouldBe`
          [ ("auth_signature", "da454824c97ba181a32ccc17a72625ba02771f50b50e1e7430e47a1f3f457e6c")
          , ("auth_key","278d425bdf160c739803")
          , ("auth_timestamp","1353088179")
          , ("auth_version","1.0")
          , ("body_md5","ec365a775a4cd0599faeb73354201b6f")
          ]

  describe "Auth.authenticatePrivate" $
    it "works" $
      -- Data from https://pusher.com/docs/auth_signatures#worked-example
      authenticatePrivate (pusher'credentials pusher) "1234.1234" "private-foobar"
      `shouldBe` "278d425bdf160c739803:58df8b0c36d6982b82c3ecf6b4662e34fe8c25bba48f5369f135bf843651c3a4"

  describe "Auth.authenticatePresenceWithEncoder" $
    it "works for presence channels" $
      -- Data from https://pusher.com/docs/auth_signatures#presence
      let
        userData = "{\"user_id\":10,\"user_info\":{\"name\":\"Mr. Pusher\"}}"
      in
        authenticatePresenceWithEncoder
          (const userData)
          (pusher'credentials pusher)
          "1234.1234"
          "presence-foobar"
          ("doesn't matter" :: String)
        `shouldBe` "278d425bdf160c739803:afaed3695da2ffd16931f457e338e6c9f2921fa133ce7dac49f529792be6304c"

pusher :: Pusher
pusher = Pusher
  { pusher'host = "http://api.pusherapp.com"
  , pusher'path = "tes/test"
  , pusher'credentials = Credentials
    { credentials'appID = 3
    , credentials'appKey = "278d425bdf160c739803"
    , credentials'appSecret = "7ad3773142a6692b25b8"
    }
  }

