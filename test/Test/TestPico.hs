module Test.TestPico where

import Test.Hspec
import Debug.Trace
import Data.Time.Hora.Parse
import Data.Time.Hora.Future as F
import Data.Time.Clock
import Data.Time.Hora.Type.DatePart
import Data.Time.Hora.Type.Time
import Data.Time.LocalTime
import Data.Time.Hora.Timestamp
import Prelude as P

main::IO()
main = hspec $ do
       describe "Test.TestPico" $ do
          it "now" $ do
            now >>= traceIO . show
            1 `shouldBe` 1
          it "parse DmyHmsFormat" $ do
            getCurrentTime >>= pure . parse >>= \(f1::DatePart Int) -> traceIO $ show f1 
            1 `shouldBe` 1
          it "parse DmyHmP" $ do
            getCurrentTime >>= pure . parse >>= \(f1::DatePart Int) -> traceIO $ show f1
            1 `shouldBe` 1
          it "parse' DmyHmP curr timezone" $ do
            getCurrentTime >>= \t1 -> do
                z1 <- getCurrentTimeZone 
                let f1 = parse' z1 t1
                traceIO $ show f1
            1 `shouldBe` 1
          it "- pico" $ do
            t1 <- getCurrentTime 
            t2 <- getCurrentTime
            let d1 = parse t1::DatePart Integer
                d2 = parse t2::DatePart Integer
                diff1 = t2 F.- t1
                diff2 = pico d2 P.- (pico d1)            
            diff1 `shouldBe` (Pico diff2)
            