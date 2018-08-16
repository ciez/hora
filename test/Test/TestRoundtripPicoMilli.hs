module Test.TestRoundtripPicoMilli where

import Data.Time.Clock
import Data.Time.Hora.Part
import Data.Time.Hora.Span
import Data.Time.Hora.Stamp
import Data.Time.Hora.Type
import Data.Word
import Debug.Trace
import Test.Hspec


main::IO()
main = hspec $ do
   describe "TestRoundtripPicoMilli" $ do
      it "round trip pico" $ do
         now1 <- now::IO UTCTime
         let dp1 = fromUtc now1::DatePart Int
             pico1 = pico dp1::Int
             milli2 = fromSec3 + fromPico3::Word32

             fromSec3 = toMilli (Sec $ second dp1)::Word32
             fromPico3 = toMilli $ Pico pico1
         tr1 "pico1: " pico1
         tr1 "sec1: " $ second dp1
         tr1 "milli2: " milli2

        --  back
         let pico5 = toPico $ Milli milli2::Int
         tr1 "pico5: " pico5
         1 `shouldBe` 1
      where tr1 desc1 data1 = traceIO $ "TestRoundtripPicoMilli " <> desc1 <> show data1
