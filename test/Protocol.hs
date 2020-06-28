module Protocol where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Network.Pusher.Protocol
  ( ChannelInfo (..),
    ChannelsInfo (..),
    FullChannelInfo (..),
    User (..),
    Users (..),
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), elements)

newtype TestChannel
  = TestChannel T.Text
  deriving (Show)

instance Arbitrary TestChannel where
  arbitrary = do
    channelPrefix <- elements ["public", "private", "presence"]
    channelSuffix <- T.pack <$> arbitrary
    return $ TestChannel $ channelPrefix <> "-" <> channelSuffix

test :: Spec
test = do
  describe "Protocol.ChannelsInfo"
    $ it "parsing works"
    $
    -- Data from https://pusher.com/docs/channels/library_auth_reference/rest-api#successful-response
    A.decode
      "{\
      \  \"channels\": {\
      \    \"presence-foobar\": {\
      \      \"user_count\": 42\
      \    },\
      \    \"presence-another\": {\
      \      \"user_count\": 123\
      \    }\
      \  }\
      \}"
      `shouldBe` ( Just
                     $ ChannelsInfo
                     $ HM.fromList
                       [ ("presence-foobar", ChannelInfo $ Just 42),
                         ("presence-another", ChannelInfo $ Just 123)
                       ]
                 )
  describe "Protocol.FullChannelInfo"
    $ it "parsing works"
    $
    -- Data from https://pusher.com/docs/channels/library_auth_reference/rest-api#successful-response
    A.decode
      "{\
      \  \"occupied\": true,\
      \  \"user_count\": 42,\
      \  \"subscription_count\": 42\
      \}"
      `shouldBe` (Just $ FullChannelInfo True (Just 42) (Just 42))
  describe "Protocol.Users"
    $ it "parsing works"
    $ A.decode
      "{\
      \  \"users\": [\
      \    { \"id\": \"1\" },\
      \    { \"id\": \"2\" }\
      \  ]\
      \}"
      `shouldBe` (Just $ Users [User "1", User "2"])
