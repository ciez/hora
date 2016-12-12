{-# LANGUAGE NoOverloadedStrings #-}
module Test.TestTime where

import Test.Hspec
import Debug.Trace
import Data.Time.Hora.Timestamp
import Text.Regex.Do.Replace.Template 
import Prelude hiding ((<),(>))
import Data.Time.Hora.Type.DmyHm
import Data.Time.Clock
import Data.Time.Hora.Format


main::IO()
main = hspec $ do
       describe "Test.TestTime" $ do
          it "timestamp" $ do
            t >>= traceIO
            1 `shouldBe` 1
          it "timestamp'" $ do
            t' >>= traceIO
            1 `shouldBe` 1
          it "datetimestamp" $ do
            dt >>= traceIO
            1 `shouldBe` 1
          it "ymd_local" $ do
            d >>= traceIO . show
            1 `shouldBe` 1
          it "formatUTCTime" $ do
            t1 <- getCurrentTime 
            let p1@DmyHm{..} = partFormats
            traceIO $ formatUTCTime (("{day}/{month} {hour}:{minute}") < [("day",day),("month",month),("hour",hour),("minute",minute)])
                        t1  
            1 `shouldBe` 1                                    