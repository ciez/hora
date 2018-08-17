module Test.TestDatePartSmallDoc where

import Data.Maybe
import Data.Semigroup hiding (Min)
import Data.Time.Clock
import Data.Time.Hora.Part
import Data.Time.Hora.Format
import Data.Time.Hora.Type
import Data.Word
import Debug.Trace
import Prelude
import Test.Hspec


main::IO()
main = hspec $ do
   describe "TestDatePartSmall" $ do
      it "day 1" $ do
           mkDay 1 1 1 `shouldBe` (Day 1)
      it "Modified Julian day" $ do
           mkDay 1858 11 17 `shouldBe` (Day julian_day_offset)
      it "mkDay" $ do
           mkDay 2018 08 16 `shouldBe` (Day 736922)
      it "max day" $ do
           let d1 = Day (maxBound::Word32)
               Just utc2 = toUtc $ d1 <> (Time 0 0)
           traceIO $ "max date: " <> (show utc2)
           1 `shouldBe` 1
      it "round trip DatePartSmall <-> UTCTime <-> DatePart" $ do
           let d1 = mkDay 2018 8 16::DatePartSmall
               dt1 = d1 <> Time 0 0::DatePartSmall
               utc2 = fromJust $ toUtc dt1::UTCTime
               dp3 = fromUtc utc2
               DatePartSmall d4 _ _ = fromUtc utc2::DatePartSmall
           year dp3 `shouldBe` 2018
           month dp3 `shouldBe` 8
           day dp3 `shouldBe` 16
           d4 `shouldBe` 736922
   describe "TestDatePartSmall show results" $ do
      it "display " $ do
         show' (mkDay 2018 8 20 <> mkMin 10 2 <> mkMs 30 9) `shouldBe` "2018-08-20 10:02:30.009"
         show' (mkDay 2018 1 1 <> Day' 20) `shouldBe` "2018-01-21"
         show' (mkDay 2018 1 1 <> Day' 180) `shouldBe` "2018-06-30"
         show' (mkMin 3 15 <> Min' 200) `shouldBe` "06:35"
         show' (normalize $ mkMin 14 59 <> mkMs 132 9 <> Ms' 5308) `shouldBe` "15:01:17.317"
         show' (mkMs 132 9) `shouldBe` "132.009"
         show' (mkMs 132 9 <> Ms' 5300) `shouldBe` "137.309"
