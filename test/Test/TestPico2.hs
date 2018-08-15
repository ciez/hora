module Test.TestPico2 where

import Data.Fixed
import Debug.Trace
import Test.Hspec


main::IO()
main = hspec $ do
       describe "Test.TestPico2" $ do
          it "case 1" $ do
            traceIO $ show p1
            let MkFixed p2 = p1
            p2 `shouldBe` 33458700000000
            where p1 = MkFixed 33458700000000::Fixed E12
