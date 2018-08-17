module Test.TestCheckOverflow where

import Data.Word
import Data.Time.Hora.Internal.DatePartSmall
import Test.Hspec


main::IO()
main = hspec $ do
   describe "TestCheckOverflow" $ do
      it "addition succeeds" $ do
         let result1 = checkOverflow Min' 3 (+) 10
         result1 `shouldBe` (Min' 13)
      it "addition maxes out" $ do
         let result1 = checkOverflow Min' max16 (+) 10
         result1 `shouldBe` (Error Overflow)
      it "negation maxes out" $ do
         let result1 = checkOverflow Min' 20 (-) 100
         result1 `shouldBe` (Error Overflow)
        where max16 = maxBound::Word16
