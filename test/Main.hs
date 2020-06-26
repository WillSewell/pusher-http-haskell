module Main where

import qualified Auth
import qualified Protocol
import Test.Hspec (hspec)
import qualified Webhook

main :: IO ()
main = hspec $ Auth.test >> Protocol.test >> Webhook.test
