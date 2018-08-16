module Test.TestDatePartSmall where

import Data.Semigroup hiding (Min)
import Data.Time.Hora.Type
import Prelude
import Test.Hspec


main::IO()
main = hspec $ do
   describe "TestDatePartSmall" $ do
      it "Day <> Time" $ do
           Day 2 <> Time 3 5 `shouldBe` DatePartSmall 2 3 5
      it "Min <> Ms" $ do
           Min 2 <> Ms 3 `shouldBe` Time 2 3
