module Main where

import Test.Hspec (hspec)

import qualified Auth
import qualified HTTP
import qualified Protocol

main :: IO ()
main = hspec $ Auth.test >> HTTP.test >> Protocol.test
