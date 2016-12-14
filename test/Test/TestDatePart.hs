module Test.TestDatePart where

import Test.Hspec
import Prelude hiding (until)
import Data.Time.Hora.Type.DatePart
default (Int) 


main::IO()
main = hspec $ do
       describe "Test.TestDatePart" $ do
          it "Ord" $ do
            (dp 1 1 1 1 1 1 1)  `shouldSatisfy` (== (dp 1 1 1 1 1 1 1))
            (dp 1 1 1 1 1 1 1)  `shouldSatisfy` (<= (dp 1 1 1 1 1 1 1))
            (dp 1 1 1 1 1 1 1)  `shouldSatisfy` (>= (dp 1 1 1 1 1 1 1))
            (dp 1 1 1 1 1 1 2)  `shouldSatisfy` (> (dp 1 1 1 1 1 1 1))
            (dp 1 1 1 1 1 1 1)  `shouldSatisfy` (<= (dp 1 1 1 1 1 1 2))
            (dp 2 1 1 1 1 1 1)  `shouldSatisfy` (< (dp 2 1 1 1 1 1 2))
            (dp 1 1 1 2 1 1 1)  `shouldSatisfy` (< (dp 1 1 1 3 1 1 1))
            (dp 1 1 1 1 3 1 1)  `shouldSatisfy` (> (dp 1 1 1 1 1 1 1))
          it "Ord" $ do
            now `shouldSatisfy` (< until)
            until `shouldSatisfy` (< stop)


now::DatePart Int
now = DatePart {year = 2016, month = 12, day = 14, hour = 11, minute = 53, second = 38, pico = 413037964000}

until::DatePart Int
until = DatePart {year = 2016, month = 12, day = 14, hour = 11, minute = 55, second = 35, pico = 754503772000}

stop:: DatePart Int
stop = DatePart {year = 2016, month = 12, day = 14, hour = 11, minute = 55, second = 45, pico = 754504065000}


dp::Int -> Int -> Int -> Int -> Int -> Int -> Int -> DatePart Int
dp = DatePart