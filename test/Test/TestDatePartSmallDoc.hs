module Test.TestDatePartSmallDoc where

import Data.Maybe
import Data.Semigroup hiding (Min)
import Data.Time.Clock
import Data.Time.Hora.Part
import Data.Time.Hora.Type
import Prelude
import Test.Hspec


main::IO()
main = hspec $ do
   describe "TestDatePartSmall" $ do
      it "ymd" $ do
           mkDay 2018 08 16 `shouldBe` (Just $ Day 736924)
      it "round trip DatePartSmall <-> UTCTime <-> DatePart" $ do
           let d1 = fromJust $ mkDay 2018 8 16::DatePartSmall
               dt1 = d1 <> Time 0 0::DatePartSmall
               utc2 = fromJust $ toUtc dt1::UTCTime
               dp3 = fromUtc utc2
               DatePartSmall d4 _ _ = fromUtc utc2::DatePartSmall
           year dp3 `shouldBe` 2018
           month dp3 `shouldBe` 8
           day dp3 `shouldBe` 16
           d4 `shouldBe` 736924

