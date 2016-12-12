module Test.TestDmyHm where

import Test.Hspec
import Data.Time.Hora.Type.DmyHm


main::IO()
main = hspec $ do
       describe "Test.TestDmyHm" $ do
          it "case 1" $ do
            a1 `shouldSatisfy` (< b1)
        where a1 = DmyHm {
                           year = 2016,
                           month = 10,
                           day = 3,
                           hour = 15,
                           minute = 23
                         }::DmyHm Int
              b1 = DmyHm {
                           year = 2016,
                           month = 10,
                           day = 3,
                           hour = 15,
                           minute = 24
                         }::DmyHm Int
