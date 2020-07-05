module Auth where

import Network.Pusher (Token (..))
import Network.Pusher.Internal.Auth
  ( authenticatePresenceWithEncoder,
    authenticatePrivate,
    makeQS,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

test :: Spec
test = do
  describe "Auth.makeQS"
    $ it "works"
    $
    -- Happy case based on data from the docs: https://pusher.com/docs/channels/library_auth_reference/auth-signatures#worked-example
    let body =
          "{\"name\":\"foo\",\"channels\":[\"project-3\"],\"data\":\"{\\\"some\\\":\\\"data\\\"}\"}"
     in makeQS
          token
          "POST"
          "/apps/3/events"
          []
          body
          1353088179
          `shouldBe` [ ( "auth_signature",
                         Just "da454824c97ba181a32ccc17a72625ba02771f50b50e1e7430e47a1f3f457e6c"
                       ),
                       ("auth_key", Just "278d425bdf160c739803"),
                       ("auth_timestamp", Just "1353088179"),
                       ("auth_version", Just "1.0"),
                       ("body_md5", Just "ec365a775a4cd0599faeb73354201b6f")
                     ]
  describe "Auth.authenticatePrivate"
    $ it "works"
    $
    -- Data from https://pusher.com/docs/channels/library_auth_reference/auth-signatures#worked-example
    authenticatePrivate token "1234.1234" "private-foobar"
      `shouldBe` "278d425bdf160c739803:58df8b0c36d6982b82c3ecf6b4662e34fe8c25bba48f5369f135bf843651c3a4"
  describe "Auth.authenticatePresenceWithEncoder"
    $ it "works for presence channels"
    $
    -- Data from https://pusher.com/docs/channels/library_auth_reference/auth-signatures#presence-channels
    let userData = "{\"user_id\":10,\"user_info\":{\"name\":\"Mr. Pusher\"}}"
     in authenticatePresenceWithEncoder
          (const userData)
          token
          "1234.1234"
          "presence-foobar"
          ("doesn't matter" :: String)
          `shouldBe` "278d425bdf160c739803:afaed3695da2ffd16931f457e338e6c9f2921fa133ce7dac49f529792be6304c"

token :: Token
token =
  Token
    { pusherKey = "278d425bdf160c739803",
      pusherSecret = "7ad3773142a6692b25b8"
    }
