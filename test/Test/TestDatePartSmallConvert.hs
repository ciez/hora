module Test.TestDatePartSmallConvert where

import Data.Maybe
import Data.Time.Clock
import Data.Time.Hora.Part
import Data.Time.Hora.Stamp
import Data.Time.Hora.Type
import Data.Time.Hora.Future as F
import Debug.Trace
import Prelude as P
import Test.Hspec

main::IO()
main = hspec $ do
   describe "TestDatePartSmallConvert" $ do
      it "now <-> DatePartSmall" $ do
         now1 <- now::IO UTCTime
         let small1 = fromUtc now1::DatePartSmall
             Just utc2 = toUtc small1
             span3 = utc2 F.- now1::TimeSpan Integer
         traceIO $ show small1
         span3 `shouldSatisfy` (< Milli 1)
