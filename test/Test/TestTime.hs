{-# LANGUAGE NoOverloadedStrings #-}
module Test.TestTime where

import Data.Time.Hora.Some
import Data.Time.Hora.Stamp
import Debug.Trace
import Test.Hspec


main::IO()
main = hspec $ do
       describe "Test.TestTime" $ do
          it "timestamp" $ do
            ts t >>= traceIO
            1 `shouldBe` 1
          it "timestamp'" $ do
            ts tf >>= traceIO
            1 `shouldBe` 1
          it "datetimestamp" $ do
            ts dt >>= traceIO
            1 `shouldBe` 1
