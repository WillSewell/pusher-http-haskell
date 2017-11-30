module Main where

import Test.Hspec (hspec)

import qualified Auth
import qualified Protocol
import qualified Push
import qualified Webhook

main :: IO ()
main = hspec $ Auth.test >> Protocol.test >> Push.test >> Webhook.test
