module Test.TestAncientDate where

import Data.Maybe
import Data.Time.Hora.Part
import Data.Time.Hora.Type
import Debug.Trace
import Test.Hspec

main::IO()
main = hspec $ do
   describe "day way in the past" $ do
      it "year 1" $ do
         traceIO $ "year 1: " <> (show utc1)
         traceIO $ "year 1: " <> (show utcbin1)
         1 `shouldBe` 1
      it "year 500" $ do
         isJust utc2 `shouldBe` True
         dp3 `shouldBe` (dp1 500)
        where utc1 = fromJust $ toUtc $ dp1 1
              utcbin1 = fromUtc utc1::UTCTimeBin
              dp1 y1 = DatePart {
                        year = y1,
                        month = 1,
                        day = 1,
                        hour = 0,
                        minute = 0,
                        second = 0,
                        pico = 0
                             }::DatePart Int
              utc2 = toUtc $ dp1 500
              dp3 = fromUtc $ fromJust utc2::DatePart Int
