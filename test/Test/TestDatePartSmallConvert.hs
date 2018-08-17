module Test.TestDatePartSmallConvert where

import Data.Maybe
import Data.Time.Clock
import Data.Time.Hora.Future as F
import Data.Time.Hora.Internal.DatePartSmall
import Data.Time.Hora.Internal.Span
import Data.Time.Hora.Part
import Data.Time.Hora.Stamp
import Data.Time.Hora.Type
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
         abs span3 `shouldSatisfy` (< (Milli 1))
      it "normalize DatePartSmall" $ do
         normalize (Time 0 $ toMilli $ Sec 62) `shouldBe` (Time 1 $ toMilli $ Sec 2)
         normalize (DatePartSmall 0 (60 * 24 + 10) $ toMilli $ Sec 62) `shouldBe` (DatePartSmall 1 11 $ toMilli $ Sec 2)
      it "increment DatePartSmall by 1 day in minutes" $ do
         now1 <- now::IO UTCTime
         let small1 = fromUtc now1::DatePartSmall
             incremented1 = small1 <> (toSpan $ mkMin 24 0)
             Just utcNow2 = toUtc small1
             Just utcIncr3 = toUtc incremented1
             dp2 = fromUtc utcNow2::DatePart Int
             dp3 = fromUtc utcIncr3::DatePart Int
         traceIO $ "increment DatePartSmall by 1 day in minutes dp2: " <> show dp2
         traceIO $ "increment DatePartSmall by 1 day in minutes dp3: " <>show dp3
         day dp3 P.- day dp2 `shouldBe` 1
