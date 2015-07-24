module Protocol where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import qualified Data.Text as T

import Network.Pusher.Protocol
  ( Channel(..)
  , ChannelType(Public, Private)
  , parseChannel
  )

test :: Spec
test =
  describe "Protocol.Channel" $ do
    it "show instance works for public channels" $
      show (Channel Public "test") `shouldBe` "test"

    it "show instance works for private channels" $
      show (Channel Private "test") `shouldBe` "private-test"

    it "show instance is an inverse of parseChannel" $
      property $ \chan -> parseChannel (T.pack $ show chan) == chan
