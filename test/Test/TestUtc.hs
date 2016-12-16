module Test.TestUtc where

import Test.Hspec
import Data.Time.Hora.Part
import Data.Time.Clock
import Data.Time.Hora.Type
import Data.Time.LocalTime
import Debug.Trace


main::IO()
main = hspec $ do
       describe "Test.TestUtc" $ do
          it "invalid date" $ do
            toUtc (DatePart {year = 2010, month = 20, day = 1, hour = 0, minute = 1 ,second = 1, pico = 0})
                `shouldBe` Nothing
          it "round trip" $ do
            now1 <- getCurrentTime
            let dp1 = fromUtc now1
                Just now2 = toUtc $ traceShow dp1 dp1   
            now1 `shouldBe` now2
          it "round trip local" $ do
            now1 <- getCurrentTime
            z1 <- getCurrentTimeZone
            let dp1 = fromUtc' z1 now1::Tz (DatePart Int)
                Just now2 = toUtc $ traceShow dp1 dp1
            now1 `shouldBe` now2
            