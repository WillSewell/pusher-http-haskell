module Main where

import Test.Hspec (hspec)

import qualified Auth

main :: IO ()
main = hspec Auth.test
