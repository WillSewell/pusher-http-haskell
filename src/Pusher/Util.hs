module Pusher.Util where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

failExpectObj :: A.Value -> A.Parser a
failExpectObj = fail . ("Expected JSON object, got " ++) . show
