module Main where

import Control.Monad.Reader (runReader)
import Test.Hspec (describe, hspec, it, shouldBe)

import Data.Pusher (Credentials(..), Pusher(..))
import Pusher.Auth (makeQS)

main :: IO ()
main = hspec $
  describe "Auth.makeQS" $
    it "sorts the query string parameters" $
      runReader (makeQS "GET" "foo/bar" [("k", "v")] "body" 1000) pusher `shouldBe` [("x", "y")]

pusher :: Pusher
pusher = Pusher
  { pusher'host = "http://api.pusherapp.com"
  , pusher'path = "tes/test"
  , pusher'credentials = Credentials
    { credentials'appID = 100000
    , credentials'appKey = "sdfsdagfdgdfhggfd"
    , credentials'appSecret = "34jh5g34jh5g435345"
    }
  }
