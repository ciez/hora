module Test.TestDiffTime where

import Test.Hspec
import Debug.Trace
import Data.Time.Clock
import Control.Concurrent
import Data.Time.Hora.Timestamp
import Data.Time.Hora.Future as F
import Data.Time.Hora.Type.Time



main::IO()
main = hspec $ do
       describe "TestDiffTime" $ do
          it "diff time" $ do
            t1 <- getCurrentTime
            threadDelay 7000
            t2 <- getCurrentTime
            let diff3 = t1 F.- t2
            traceIO $ show diff3
            1 `shouldBe` 1
          it "DmyHmP" $ do
            dmy2 <- now
            traceIO $ "TestDiffTime" ++ (show dmy2)
            1 `shouldBe` 1
          it "dmy_local" $ do
            Tz tz1 l1 <- d'
            traceIO $ show l1
            1 `shouldBe` 1
