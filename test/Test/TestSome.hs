module Test.TestSome where

import Data.Time.Hora.Some
import Data.Time.Hora.Stamp
import Data.Time.Hora.Type
import Debug.Trace
import Test.Hspec


main::IO()
main = hspec $ do
       describe "Test.TestCommon" $ do
          it "iso" $ do
            ts iso >>= utc 
            1 `shouldBe` 1
          it "ymd" $ do
            ts ymd >>= utc
            1 `shouldBe` 1
          it "tf" $ do
            ts tf >>= utc
            1 `shouldBe` 1
          it "t" $ do
            ts t >>= utc
            1 `shouldBe` 1
          it "dt" $ do
            ts dt >>= utc
            1 `shouldBe` 1


utc::Tz String -> IO()
utc = traceIO . show            
