module Auth where

import Test.Hspec (Spec, describe, it, shouldBe)

import Network.Pusher
       (Channel(..), ChannelType(Presence, Private), Credentials(..))
import Network.Pusher.Internal.Auth
       (authenticatePresenceWithEncoder, authenticatePrivate, makeQS)

test :: Spec
test = do
  describe "Auth.makeQS" $
    it "works" $
      -- Happy case based on data from the docs: https://pusher.com/docs/rest_api#worked-authentication-example
    let body =
          "{\"name\":\"foo\",\"channels\":[\"project-3\"],\"data\":\"{\\\"some\\\":\\\"data\\\"}\"}"
    in makeQS
         (credentialsAppKey credentials)
         (credentialsAppSecret credentials)
         "POST"
         "/apps/3/events"
         []
         body
         1353088179 `shouldBe`
       [ ( "auth_signature"
         , "da454824c97ba181a32ccc17a72625ba02771f50b50e1e7430e47a1f3f457e6c")
       , ("auth_key", "278d425bdf160c739803")
       , ("auth_timestamp", "1353088179")
       , ("auth_version", "1.0")
       , ("body_md5", "ec365a775a4cd0599faeb73354201b6f")
       ]
  describe "Auth.authenticatePrivate" $
    it "works" $
      -- Data from https://pusher.com/docs/auth_signatures#worked-example
    authenticatePrivate credentials "1234.1234" (Channel Private "foobar") `shouldBe`
    "278d425bdf160c739803:58df8b0c36d6982b82c3ecf6b4662e34fe8c25bba48f5369f135bf843651c3a4"
  describe "Auth.authenticatePresenceWithEncoder" $
    it "works for presence channels" $
      -- Data from https://pusher.com/docs/auth_signatures#presence
    let userData = "{\"user_id\":10,\"user_info\":{\"name\":\"Mr. Pusher\"}}"
    in authenticatePresenceWithEncoder
         (const userData)
         credentials
         "1234.1234"
         (Channel Presence "foobar")
         ("doesn't matter" :: String) `shouldBe`
       "278d425bdf160c739803:afaed3695da2ffd16931f457e338e6c9f2921fa133ce7dac49f529792be6304c"

credentials :: Credentials
credentials =
  Credentials
  { credentialsAppID = 3
  , credentialsAppKey = "278d425bdf160c739803"
  , credentialsAppSecret = "7ad3773142a6692b25b8"
  , credentialsCluster = Nothing
  }
