{-# LANGUAGE NoOverloadedStrings #-}
module Test.TestTime where

import Test.Hspec
import Debug.Trace
import Data.Time.Hora.Stamp
import Data.Time.Hora.Some


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
