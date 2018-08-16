module Test.TestUTCTimeBin where

import Data.Binary
import Data.ByteString.Lazy
import Data.Maybe
import Data.Time.Clock
import Data.Time.Hora.Part
import Data.Time.Hora.Stamp
import Data.Time.Hora.Type
import Debug.Trace
import Test.Hspec


main::IO()
main = hspec $ do
   describe "round trip convert & serialize UTCTime via UTCTimeBin" $ do
      it "round trip convert" $ do
         utc1 <- now::IO UTCTime
         let utcbin2 = fromUtc utc1::UTCTimeBin
             utc2 = fromJust $ toUtc utcbin2
         traceIO $ show utc1
         traceIO $ show utcbin2
         traceIO $ show utc2
         utc2 `shouldBe` utc1
      it "round trip convert & serialize" $ do
         utc10 <- now::IO UTCTime
         let utcbin10 = fromUtc utc10::UTCTimeBin
             bin11 = encode utcbin10::ByteString
             utcbin12 = decode bin11::UTCTimeBin
             utc13 = fromJust $ toUtc utcbin12
         utc10 `shouldBe` utc13
