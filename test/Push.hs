module Push where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
       (Arbitrary(..), Gen, elements, oneof, property, vectorOf)

import Network.Pusher

test :: Spec
test = do
  describe "Interest names" $ do
    it "are accepted when using valid characters" $
      property $ do
        txt <- arbitraryInterestText
        return $
          case mkInterest txt of
            Nothing -> False
            Just _ -> True
    it "are rejected when using invalid characters" $
      property $ do
        txt <- arbitraryInvalidInterestText
        return $
          case mkInterest txt of
            Nothing -> True
            Just _ -> False
  describe "Notification JSON parser" $ do
    it "correctly parses a specific FCM Notification." $
      property $
      let (inputBS, expected) = validFCMDecoding
      in A.decode inputBS == Just expected
    it "Random Notifications JSON parses correctly" $
      property $ \(NotificationDecoding (bs, notification)) ->
        A.decode bs == Just notification
  describe "Notification JSON encoder" $ do
    it "encodes a specific FCM Notification correctly" $
      property $
      let (bs, notification) = validFCMDecoding
      in (A.encode notification) ==
         (A.encode . fromJust . (A.decode :: ByteString -> Maybe Notification) $
          bs)
    it "encodes random Notifications correctly" $
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
        "{\
          \ \"interests\": [\"interestOne\"]\
          \ ,\"fcm\": {\
          \     \"notification\":\
          \        {\"title\":\"titleText\"\
          \        ,\"body\":\"bodyText\"\
          \        }\
          \  }\
          \}"
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
  arbitrary = do
    InterestText titleText <- arbitrary
    InterestText bodyText <- arbitrary
    NonRecJSON dataJSON <- arbitrary
    let alert :: A.Object
        alert = HM.fromList ["title" .= titleText, "body" .= bodyText]
        aps :: A.Object
        aps = HM.fromList ["alert" .= alert]
        payload :: A.Object
        payload = HM.fromList ["aps" .= aps, "data" .= dataJSON]
    let apns = APNSPayload payload
        bs :: ByteString
        bs =
          "{\
          \  \"aps\":\
          \   {\
          \     \"alert\":\
          \       {\"title\":\"" <>
          (B.pack . T.unpack $ titleText) <>
          "\"\
          \       ,\"body\":\"" <>
          (B.pack . T.unpack $ bodyText) <>
          "\"\
          \       }\
          \   }\
          \   ,\"data\":" <>
          (A.encode dataJSON) <>
          "\
          \}"
    return $ APNSDecoding (bs, Just apns)

instance Arbitrary GCMDecoding where
  arbitrary = do
    InterestText titleText <- arbitrary
    InterestText bodyText <- arbitrary
    NonRecJSON dataJSON <- arbitrary
    let notification :: A.Object
        notification = HM.fromList ["title" .= titleText, "body" .= bodyText]
        payload :: A.Object
        payload =
          HM.fromList ["notification" .= notification, "data" .= dataJSON]
        gcm = GCMPayload payload
        bs :: ByteString
        bs =
          "{\
          \  \"notification\":\
          \    {\"title\":\"" <>
          (B.pack . T.unpack $ titleText) <>
          "\"\
          \    ,\"body\":\"" <>
          (B.pack . T.unpack $ bodyText) <>
          "\"\
          \    }\
          \    ,\"data\":" <>
          (A.encode dataJSON) <>
          "\
          \}"
    return $ GCMDecoding (bs, Just gcm)

instance Arbitrary FCMDecoding where
  arbitrary = do
    InterestText titleText <- arbitrary
    InterestText bodyText <- arbitrary
    NonRecJSON dataJSON <- arbitrary
    let notification :: A.Object
        notification = HM.fromList ["title" .= titleText, "body" .= bodyText]
        payload :: A.Object
        payload =
          HM.fromList ["notification" .= notification, "data" .= dataJSON]
        gcm = FCMPayload payload
        bs :: ByteString
        bs =
          "{\
          \  \"notification\":\
          \    {\"title\":\"" <>
          (B.pack . T.unpack $ titleText) <>
          "\"\
          \    ,\"body\":\"" <>
          (B.pack . T.unpack $ bodyText) <>
          "\"\
          \    }\
          \    ,\"data\":" <>
          (A.encode dataJSON) <>
          "\
          \}"
    return $ FCMDecoding (bs, Just gcm)

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
        consOptionals ::
             [(ByteString, ByteString)] -> [(ByteString, ByteString)]
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
      consJust ::
           ByteString
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

-- JSON but with no nested objects or nested arrays to make generating arbitrary
-- structures simpler. A better solution would be to used 'sized' or similar to
-- limit the size.
newtype NonRecJSON =
  NonRecJSON A.Value

instance Arbitrary NonRecJSON where
  arbitrary =
    NonRecJSON <$>
    oneof [arbitraryJSONPrimitives, arbitraryObject, arbitraryArray]
    where
      arbitraryObject = A.Object . _unNonRecObject <$> arbitrary
      arbitraryArray = A.Array . _unNonRecArray <$> arbitrary

-- Strings, numbers, bools and null. No arrays or objects
arbitraryJSONPrimitives :: Gen A.Value
arbitraryJSONPrimitives =
  oneof [arbitraryString, arbitraryNumber, arbitraryBool, arbitraryNull]
  where
    arbitraryString = A.String . _unInterestText <$> arbitrary
    arbitraryNumber = A.Number . _unOurNumber <$> arbitrary
    arbitraryBool = A.Bool <$> arbitrary
    arbitraryNull = pure A.Null

-- Valid interest text has a certain size and character set although we use
-- this as a general source of 'safe' random text elsewhere.
newtype InterestText = InterestText
  { _unInterestText :: T.Text
  }

instance Arbitrary InterestText where
  arbitrary =
    InterestText <$> do
      n <- elements [1 .. 164]
      str <-
        vectorOf n $
        elements $
        concat
          [ ['a' .. 'z']
          , ['A' .. 'Z']
          , ['0' .. '1']
          , ['_', '=', '@', ',', '.', ';']
          ]
      return . T.pack $ str

-- The internal type of JSON objects is hashmap of text keys to values.
-- We only nest primitives.
newtype NonRecObject = NonRecObject
  { _unNonRecObject :: HM.HashMap T.Text A.Value
  }

instance Arbitrary NonRecObject where
  arbitrary =
    NonRecObject <$> do
      o <-
        vectorOf 10 $ do
          InterestText k <- arbitrary
          v <- arbitraryJSONPrimitives
          return (k, v)
      return . HM.fromList $ o

-- The internal type of JSON arrays is a Vector of values.
-- We only nest primitives.
newtype NonRecArray = NonRecArray
  { _unNonRecArray :: V.Vector A.Value
  }

instance Arbitrary NonRecArray where
  arbitrary =
    NonRecArray <$> do
      xs <- vectorOf 10 arbitraryJSONPrimitives
      return . V.fromList $ xs

-- The internal type of JSON numbers is a Scientific number.
-- We generate arbitrary values with the 'scientific' function.
newtype OurNumber = OurNumber
  { _unOurNumber :: Scientific
  }

instance Arbitrary OurNumber where
  arbitrary = OurNumber <$> (scientific <$> arbitrary <*> arbitrary)
