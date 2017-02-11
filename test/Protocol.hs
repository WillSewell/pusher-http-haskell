module Protocol where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary(..), elements, property)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Network.Pusher
  ( Channel(..)
  , ChannelType(Public, Presence, Private)
  , parseChannel
  , renderChannel
  , Notification
  , parseInterest
  )
import Network.Pusher.Protocol
  ( ChannelInfo(..)
  , ChannelsInfo(..)
  , FullChannelInfo(..)
  , User(..)
  , Users(..)
  )

newtype TestChannel = TestChannel Channel deriving (Show)

instance Arbitrary TestChannel where
  arbitrary = do
    let
      testChannelType = elements [Public, Private, Presence]
      channel = Channel <$> testChannelType <*> (T.pack <$> arbitrary)
    TestChannel <$> channel

test :: Spec
test = do
  describe "Protocol.Channel" $ do
    it "show instance works for public channels" $
      renderChannel (Channel Public "test") `shouldBe` "test"

    it "show instance works for private channels" $
      renderChannel (Channel Private "test") `shouldBe` "private-test"

    it "show instance is an inverse of parseChannel" $
      property $ \(TestChannel chan) -> parseChannel (renderChannel chan) == chan

  describe "Protocol.ChannelsInfo" $
    it "parsing works" $
      -- Data from https://pusher.com/docs/rest_api#successful-response-1
      A.decode
        "{\
\         \"channels\": {\
\           \"presence-foobar\": {\
\             \"user_count\": 42\
\           },\
\           \"presence-another\": {\
\             \"user_count\": 123\
\           }\
\         }\
\       }"
      `shouldBe`
        (Just $ ChannelsInfo $ HM.fromList
          [ (Channel Presence "foobar", ChannelInfo $ Just 42)
          , (Channel Presence "another", ChannelInfo $ Just 123)
          ])

  describe "Protocol.FullChannelInfo" $
    it "parsing works" $
      -- Data from https://pusher.com/docs/rest_api#successful-response-2
      A.decode
        "{\
\         \"occupied\": true,\
\         \"user_count\": 42,\
\         \"subscription_count\": 42\
\       }"
      `shouldBe`
        (Just $ FullChannelInfo True (Just 42) (Just 42))

  describe "Protocol.Users" $
    it "parsing works" $
      A.decode
        "{\
\         \"users\": [\
\           { \"id\": \"1\" },\
\           { \"id\": \"2\" }\
\         ]\
\       }"
      `shouldBe`
        (Just $ Users [User "1", User "2"])

