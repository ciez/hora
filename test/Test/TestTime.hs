{-# LANGUAGE NoOverloadedStrings #-}
module Test.TestTime where

import Test.Hspec
import Debug.Trace
import Data.Time.Hora.Timestamp
import Prelude hiding ((<),(>))


main::IO()
main = hspec $ do
       describe "Test.TestTime" $ do
          it "timestamp" $ do
            t >>= traceIO
            1 `shouldBe` 1
          it "timestamp'" $ do
            tf >>= traceIO
            1 `shouldBe` 1
          it "datetimestamp" $ do
            dt >>= traceIO
            1 `shouldBe` 1
          it "ymd_local" $ do
            d >>= traceIO . show
            1 `shouldBe` 1
