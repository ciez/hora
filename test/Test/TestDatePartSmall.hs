module Test.TestDatePartSmall where

import Data.Semigroup hiding (Min)
import Data.Time.Hora.Type
import Prelude
import Test.Hspec


main::IO()
main = hspec $ do
   describe "TestDatePartSmall" $ do
      it "1 Day <> Time" $ do
           Day 2 <> Time 3 5 `shouldBe` DatePartSmall 2 3 5
      it "2 Min <> Ms" $ do
           Min 2 <> Ms 3 `shouldBe` Time 2 3
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
