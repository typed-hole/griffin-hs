module Main (main) where

import qualified Griffin.ApiTest as ApiTest
import           Test.Hspec      (hspec)

main :: IO ()
main = hspec
  ApiTest.tests
