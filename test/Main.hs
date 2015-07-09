module Main where

import Test.Hspec (hspec)

import qualified Auth
import qualified HTTP

main :: IO ()
main = hspec $ Auth.test >> HTTP.test
