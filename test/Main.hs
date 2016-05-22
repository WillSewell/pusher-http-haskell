module Main where

import Test.Hspec (hspec)

import qualified Auth
import qualified Protocol

main :: IO ()
main = hspec $ Auth.test >> Protocol.test
