module Test.TestDatePartSmall where

import Data.Semigroup hiding (Min)
import Data.Time.Hora.Internal.DatePartSmall
import Prelude hiding (negate)
import Test.Hspec


main::IO()
main = hspec $ do
   describe "TestDatePartSmall merge" $ do
      it "1 Day <> Time" $ do
           Day 2 <> Time 3 5 `shouldBe` DatePartSmall 2 3 5
      it "2 Min <> Ms" $ do
           Min 2 <> Ms 3 `shouldBe` Time 2 3

   describe "TestDatePartSmall increment" $ do
      it "3" $ do
           DatePartSmall 1 2 3 <> Day' 2 `shouldBe` DatePartSmall 3 2 3
      it "4" $ do
           DatePartSmall 1 2 3 <> Min' 2 `shouldBe` DatePartSmall 1 4 3
      it "5" $ do
           DatePartSmall 1 2 3 <> Ms' 2 `shouldBe` DatePartSmall 1 2 5
      it "6" $ do
           Time 2 3 <> Min' 2 `shouldBe` Time 4 3
      it "7" $ do
           Time 2 3 <> Ms' 2 `shouldBe` Time 2 5
      it "8" $ do
           Day 1 <> Day' 2 `shouldBe` Day 3
      it "9" $ do
           Min 1 <> Min' 2 `shouldBe` Min 3
      it "10" $ do
           Ms 1 <> Ms' 2 `shouldBe` Ms 3

      it "8: 2 x spans" $ do
           Day' 1 <> Day' 2 `shouldBe` Day' 3
      it "9: 2 x spans" $ do
           Min' 1 <> Min' 2 `shouldBe` Min' 3
      it "10: 2 x spans" $ do
           Ms' 1 <> Ms' 2 `shouldBe` Ms' 3

   describe "TestDatePartSmall decrement" $ do
      it "3" $ do
           DatePartSmall 10 2 3 <> negate (Day' 2) `shouldBe` DatePartSmall 8 2 3
      it "4" $ do
           DatePartSmall 1 2 3 <> negate (Min' 2) `shouldBe` DatePartSmall 1 0 3
      it "5" $ do
           DatePartSmall 1 2 3 <> negate (Ms' 2) `shouldBe` DatePartSmall 1 2 1
      it "6" $ do
           Time 20 3 <> negate (Min' 2) `shouldBe` Time 18 3
      it "7" $ do
           Time 2 30 <> negate (Ms' 5) `shouldBe` Time 2 25
      it "8" $ do
           Day 10 <> negate (Day' 2) `shouldBe` Day 8
      it "9" $ do
           Min 10 <> negate (Min' 3) `shouldBe` Min 7
      it "10" $ do
           Ms 100 <> negate (Ms' 12) `shouldBe` Ms 88

      it "8: 2 x spans" $ do
           Day' 10 <> negate (Day' 4) `shouldBe` Day' 6
      it "9: 2 x spans" $ do
           Min' 14 <> negate (Min' 2) `shouldBe` Min' 12
      it "10: 2 x spans" $ do
           Ms' 23 <> negate (Ms' 2) `shouldBe` Ms' 21

      it "8: 2 x spans" $ do
           Day' 10 <> negate (Day' 4) `shouldBe` Day' 6
      it "9: 2 x spans" $ do
           Min' 14 <> negate (Min' 2) `shouldBe` Min' 12
      it "10: 2 x spans" $ do
           Ms' 23 <> negate (Ms' 2) `shouldBe` Ms' 21

      it "8: 2 x spans" $ do
           negate (Day' 10) <> negate (Day' 4) `shouldBe` (negate $ Day' 14)
      it "9: 2 x spans" $ do
           negate (Min' 14) <> negate (Min' 2) `shouldBe` (negate $ Min' 16)
      it "10: 2 x spans" $ do
           negate (Ms' 23) <> negate (Ms' 2) `shouldBe` (negate $ Ms' 25)

   describe "TestDatePartSmall overwrite" $ do
      it "11" $ do
           DatePartSmall 1 2 3 <> DatePartSmall 5 6 7 `shouldBe` DatePartSmall 5 6 7
      it "12" $ do
           Time 2 3 <> Time 7 8 `shouldBe` Time 7 8
      it "13" $ do
           Day 3 <> Day 8 `shouldBe` Day 8
      it "14" $ do
           Min 1 <> Min 20 `shouldBe` Min 20
      it "15" $ do
           Ms 1 <> Ms 20 `shouldBe` Ms 20

   describe "TestDatePartSmall update" $ do
      it "16" $ do
           DatePartSmall 1 2 3 <> Time 5 6 `shouldBe` DatePartSmall 1 5 6
      it "17" $ do
           Time 2 3 <> Min 8 `shouldBe` Time 8 3
      it "18" $ do
           Time 2 3 <> Ms 8 `shouldBe` Time 2 8

      it "19" $ do
           Error Invalid <> Error Invalid `shouldBe` Error Invalid
      it "20" $ do
           Error Invalid <> Error Overflow `shouldBe` Error Invalid_Overflow
      it "21" $ do
           Error Overflow <> Error Overflow `shouldBe` Error Overflow
      it "22" $ do
           Error Overflow <> Error Invalid ` shouldBe ` Error Invalid_Overflow
      it "23" $ do
           Error Invalid_Overflow <> Min 1 ` shouldBe ` Error Invalid_Overflow
      it "24" $ do
           Min 1 <> Error Invalid_Overflow ` shouldBe ` Error Invalid_Overflow
      it "25" $ do
           Error Overflow <> Min 1 ` shouldBe ` Error Overflow
      it "26" $ do
           Day 3 <> Error Invalid ` shouldBe ` Error Invalid
      it "27" $ do
           Time 1 2 <> Day 3 `shouldBe` Error Invalid
