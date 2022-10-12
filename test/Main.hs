module Main (main) where

import Griffin.ApiTest qualified as ApiTest
import Test.Hspec ( hspec )

main :: IO ()
main = hspec $
  ApiTest.tests