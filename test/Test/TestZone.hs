module Test.TestZone where

import Test.Hspec
import Debug.Trace
import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import Data.Time.Hora.Zone
import Data.Time.Hora.Stamp
import Data.Time.Hora.Type

main::IO()
main = hspec $ do
       describe "Test.TestZone" $ do
          it "UTC to Chile" $ do
            chile1 <- getTimeZoneSeriesFromOlsonFile "./test/TimeSeriesData/ChileContinental"
            china1 <- getTimeZoneSeriesFromOlsonFile "./test/TimeSeriesData/ROC"
            now1 <- now::IO UTCTime 
            traceIO $ "UTC: " ++ (show now1)
            traceIO $ "Chile: " ++ (show $ ttz chile1 now1)
            traceIO $ "China: " ++ (show $ ttz china1 now1)
            1 `shouldBe` 1


ttz::TimeZoneSeries -> UTCTime -> Tz (DatePart Int)
ttz = toTimeZone            