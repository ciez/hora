module Test.TestFuture where

import Data.Time.Clock
import Data.Time.Hora.Future
import Data.Time.Hora.Type
import Debug.Trace
import Test.Hspec


main::IO()
main = hspec $ do
       describe "Case.TestFuture" $ do
          it "3 second from now" $ do
            t1 <- getCurrentTime
            traceIO $ show t1
            f2 <- futureUTCTime $ Milli 100
            traceIO $ show f2
            f3 <- futureUTCTime $ Sec 3
            traceIO $ show f3
            1 `shouldBe` 1
