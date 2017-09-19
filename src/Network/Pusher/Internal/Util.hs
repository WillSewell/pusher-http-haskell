{-|
Module      : Network.Pusher.Internal.Util
Description : Various utilty functions
Copyright   : (c) Will Sewell, 2016
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental
-}
module Network.Pusher.Internal.Util
  ( failExpectObj
  , failExpectArray
  , failExpectStr
  , failExpectSingletonArray
  , getTime
  , show'
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.String (IsString, fromString)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- |Get the system time as an Int.
getTime :: IO Int
getTime = round <$> getPOSIXTime

-- |When decoding Aeson values, this can be used to return a failing parser
-- when an object was expected, but it was a different type of data.
failExpectObj :: A.Value -> A.Parser a
failExpectObj = fail . ("Expected JSON object, got " ++) . show

-- |When decoding Aeson values, this can be used to return a failing parser
-- when an array was expected, but it was a different type of data.
failExpectArray :: A.Value -> A.Parser a
failExpectArray = fail . ("Expected JSON array, got " ++) . show

-- |When decoding Aeson values, this can be used to return a failing parser
-- when an array of length exactly one was expected but it was a different type
-- of data.
failExpectSingletonArray :: A.Value -> A.Parser a
failExpectSingletonArray =
  fail . ("Expected JSON array with exactly one object, got" ++) . show

-- |When decoding Aeson values, this can be used to return a failing parser
-- when an string was expected, but it was a different type of data.
failExpectStr :: A.Value -> A.Parser a
failExpectStr = fail . ("Expected JSON string, got " ++) . show

-- | Generalised version of show
show'
  :: (Show a, IsString b)
  => a -> b
show' = fromString . show
