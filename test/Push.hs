module Push where

import qualified Data.Text as T

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (elements, property, Gen, vectorOf, oneof)

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
