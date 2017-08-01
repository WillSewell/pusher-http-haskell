{-# LANGUAGE OverloadedStrings #-}

module Push where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
       (elements, property, Gen, vectorOf, oneof, Arbitrary(..))

import Network.Pusher

test :: Spec
test = do
  describe "Interest names are validated" $ do
    it "Correct Interest names are accepted" $
      property $ do
        txt <- arbitraryInterestText
        return $
          case mkInterest txt of
            Nothing -> False
            Just _ -> True
    it "Invalid Interest names are rejected" $
      property $ do
        txt <- arbitraryInvalidInterestText
        return $
          case mkInterest txt of
            Nothing -> True
            Just _ -> False
  describe "Notifications parse" $ do
    it "A specific FCM Notification JSON parses correctly" $
      property $
      let (inputBS, expected) = validFCMDecoding
      in A.decode inputBS == Just expected
    it "Random Notifications JSON parses correctly" $
      property $ \(NotificationDecoding (bs, notification)) ->
        A.decode bs == Just notification
  describe "Notifications encode" $ do
    it "A specific FCM Notification encodes to JSON correctly" $
      property $
      let (bs, notification) = validFCMDecoding
      in (A.encode notification) ==
         (A.encode . fromJust . (A.decode :: ByteString -> Maybe Notification) $
          bs)
    it "Random Notifications encode to JSON correctly" $
      -- NOTE: We put the expected string through a round trip to normalise trivial
      -- differences such as ordering and spacing in the JSON Aeson generates
      property $ \(NotificationDecoding (bs, notification)) ->
        (A.encode notification) ==
        (A.encode . fromJust . (A.decode :: ByteString -> Maybe Notification) $
         bs)

-- A single example of a FCM notification which we expect to aeson decode to
-- the paired Notification value.
validFCMDecoding :: (B.ByteString, Notification)
validFCMDecoding =
  let bs =
        B.unlines
          [ "{"
          , "\"interests\": [\"interestOne\"],"
          , "\"fcm\": { \"notification\": {"
          , "              \"body\": \"bodyText\","
          , "              \"title\": \"titleText\""
          , "                             }"
          , "         }"
          , "}"
          ]
      notification =
        Notification
        { notificationInterest = fromJust . mkInterest $ "interestOne"
        , notificationWebhookURL = Nothing
        , notificationWebhookLevel = Nothing
        , notificationAPNSPayload = Nothing
        , notificationGCMPayload = Nothing
        , notificationFCMPayload =
            Just $
            FCMPayload $
            HM.fromList
              [ ("notification" :: T.Text) .=
                (A.Object $
                 HM.fromList
                   [ ("title" :: T.Text) .= ("titleText" :: T.Text)
                   , ("body" :: T.Text) .= ("bodyText" :: T.Text)
                   ])
              ]
        }
  in (bs, notification)

-- | A ByteString will decode to some value 'a'.
type Decoding a = (B.ByteString, a)

newtype NotificationDecoding =
  NotificationDecoding (Decoding Notification)
  deriving (Show)

newtype APNSDecoding =
  APNSDecoding (Decoding (Maybe APNSPayload))

newtype FCMDecoding =
  FCMDecoding (Decoding (Maybe FCMPayload))

newtype GCMDecoding =
  GCMDecoding (Decoding (Maybe GCMPayload))

instance Arbitrary APNSDecoding where
  arbitrary = return $ APNSDecoding ("", Nothing)

instance Arbitrary GCMDecoding where
  arbitrary = return $ GCMDecoding ("", Nothing)

instance Arbitrary FCMDecoding where
  arbitrary = return $ FCMDecoding ("", Nothing)

instance Arbitrary NotificationDecoding where
  arbitrary = do
    interestName <- arbitraryInterestText
    APNSDecoding (apnsBS, mAPNS) <- arbitrary
    GCMDecoding (gcmBS, mGCM) <- arbitrary
    FCMDecoding (fcmBS, mFCM) <- arbitrary
    let notification =
          Notification
          { notificationInterest = fromJust . mkInterest $ interestName
          , notificationWebhookURL = Nothing
          , notificationWebhookLevel = Nothing
          , notificationAPNSPayload = mAPNS
          , notificationGCMPayload = mGCM
          , notificationFCMPayload = mFCM
          }
        -- Key value pairs that are required
        requiredFields :: [(ByteString, ByteString)]
        requiredFields =
          [("interests", "[\"" <> (B.pack . T.unpack $ interestName) <> "\"]")]
        -- A function which takes an initial key value pair list and adds Just pairs and skips Nothing values.
        -- Aeson would by default encode this as "value":null which we dont want.
        consOptionals :: [(ByteString, ByteString)]
                      -> [(ByteString, ByteString)]
        consOptionals =
          consJust "apns" (nullToMaybe apnsBS) .
          consJust "fcm" (nullToMaybe fcmBS) .
          consJust "gcm" (nullToMaybe gcmBS)
        fields :: [(ByteString, ByteString)]
        fields = consOptionals requiredFields
        bs :: B.ByteString
        bs =
          (\acc -> "{" <> acc <> "}") .
          B.intercalate "," . map (\(k, v) -> "\"" <> k <> "\": " <> v) $
          fields
    return $ NotificationDecoding (bs, notification)
      -- Cons an attribute value pair if Just
    where
      consJust
        :: ByteString
        -> Maybe ByteString
        -> [(ByteString, ByteString)]
        -> [(ByteString, ByteString)]
      consJust attr = maybe id (\val rest -> (attr, val) : rest)
      -- Empty ByteStrings become null
      nullToMaybe :: B.ByteString -> Maybe B.ByteString
      nullToMaybe bs
        | B.null bs = Nothing
        | otherwise = Just bs

-- Valid interest names
arbitraryInterestText :: Gen T.Text
arbitraryInterestText = do
  n <- elements [1 .. 164]
  str <-
    vectorOf n $
    elements $
    concat
      [['a' .. 'z'], ['A' .. 'Z'], ['0' .. '1'], ['_', '=', '@', ',', '.', ';']]
  return . T.pack $ str

-- Invalid interest names
arbitraryInvalidInterestText :: Gen T.Text
arbitraryInvalidInterestText =
  T.pack <$>
  oneof
    [tooLargeInterestText, tooSmallInterestText, invalidCharactersInterestText]
  where
    tooLargeInterestText = do
      n <- elements [165 .. 265]
      vectorOf n $
        elements $
        concat
          [ ['a' .. 'z']
          , ['A' .. 'Z']
          , ['0' .. '1']
          , ['_', '=', '@', ',', '.', ';']
          ]
    tooSmallInterestText = return ""
    invalidCharactersInterestText = do
      n <- elements [1 .. 165]
      vectorOf n $ elements "!\"£$%^&*()+}{~:?><¬` `}\""
