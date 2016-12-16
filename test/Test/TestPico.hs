module Test.TestPico where

import Test.Hspec
import Debug.Trace
import Data.Time.Hora.Part
import Data.Time.Hora.Future as F
import Data.Time.Clock
import Data.Time.Hora.Type
import Data.Time.LocalTime
import Data.Time.Hora.Stamp
import Prelude as P

main::IO()
main = hspec $ do
       describe "Test.TestPico" $ do
          it "now" $ do
            (now::IO (DatePart Int)) >>= traceIO . show
            1 `shouldBe` 1
          it "parse DmyHmsFormat" $ do
            getCurrentTime >>= pure . fromUtc >>= \(f1::DatePart Int) -> traceIO $ show f1 
            1 `shouldBe` 1
          it "parse DmyHmP" $ do
            getCurrentTime >>= pure . fromUtc >>= \(f1::DatePart Int) -> traceIO $ show f1
            1 `shouldBe` 1
          it "parse' DmyHmP curr timezone" $ do
            getCurrentTime >>= \t1 -> do
                z1 <- getCurrentTimeZone 
                let f1 = fromUtc' z1 t1
                traceIO $ show f1
            1 `shouldBe` 1
          it "- pico" $ do
            t1 <- getCurrentTime 
            t2 <- getCurrentTime
            let d1 = fromUtc t1::DatePart Integer
                d2 = fromUtc t2::DatePart Integer
                diff1 = t2 F.- t1
                diff2 = pico d2 P.- (pico d1)            
            diff1 `shouldBe` (Pico diff2)
            