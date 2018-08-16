module Test.TestDatePartSmall where

import Data.List.NonEmpty
import Data.Semigroup
import Data.Time.Hora.Type
import Prelude
import Test.Hspec


main::IO()
main = hspec $ do
   describe "combine" $ do
      it "day <> time" $ do
         let Combine result1 = sconcat $ Combine <$> d1 :| [t1]
         result1 `shouldBe` expect1
        where d1 = Day 2
              t1 = Time (3, 5)
              expect1 = DatePartSmall (2, 3, 5)
