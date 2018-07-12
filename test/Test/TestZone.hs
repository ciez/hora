module Test.TestZone where

import Test.Hspec
import Debug.Trace
import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import Data.Time.Hora.Stamp
import Data.Time.Hora.Type
import Data.Time.Hora.Part
import Data.Time.Hora.Zone


main::IO()
main = hspec $ do
       describe "Test.TestZone" $ do
          it "UTC to Chile" $ do
            chile1 <- getTimeZoneSeriesFromOlsonFile "./test/TimeSeriesData/ChileContinental"
            china1 <- getTimeZoneSeriesFromOlsonFile "./test/TimeSeriesData/ROC"
            now1 <- now::IO UTCTime 
            traceIO $ "UTC: " ++ (show now1)
            let chile2 = ttz chile1 now1
                china2 = ttz china1 now1
                china3 = toTimeZone china1 chile2
            traceIO $ "Chile: " ++ (show chile2)
            traceIO $ "China: " ++ (show china1)
            traceIO $ "China converted: " ++ (show china3)
            Just china2 `shouldBe` china3


ttz::TimeZoneSeries -> UTCTime -> Tz (DatePart Int)
ttz = fromUtc'            