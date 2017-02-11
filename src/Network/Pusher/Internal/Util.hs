{-|
Module      : Network.Pusher.Internal.Util
Description : Various utilty functions
Copyright   : (c) Will Sewell, 2016
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental
-}
module Network.Pusher.Internal.Util
  ( failExpect
  , failExpectObj
  , getTime
  , show'
  ) where

import Data.String (IsString, fromString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

-- |Get the system time as an Int.
getTime :: IO Int
getTime = round <$> getPOSIXTime

-- |When decoding Aeson values, this can be used to return a failing parser
-- when an object was expected, but it was a different type of data.
failExpectObj :: A.Value -> A.Parser a
failExpectObj = failExpect "JSON object"

failExpect :: String -> A.Value -> A.Parser a
failExpect expected = fail . (concat ["Expected ", expected, ", got "] ++) . show

-- | Generalised version of show
show' :: (Show a, IsString b) => a -> b
show' = fromString . show
