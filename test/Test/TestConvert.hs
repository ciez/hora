module Test.TestConvert where

import Test.Hspec
import Data.Time.Hora.Type.Time
import Data.Time.Hora.Convert
import Data.Time.Clock
import Data.Time.Hora.Type.DmyHm as D
import Data.Time.Hora.Type.YmdHms as Y


main::IO()
main = do 
    test
    doc



test::IO()
test = hspec $ do
       describe "Case.Convert" $ do
          it "pico 1" $ do
              toPico (Sec $ i 12) `shouldBe` (toPico $ Milli $ toMilli $ Sec $ i 12)
              toMilli (Sec $ i 17) `shouldBe` (toMilli $ Pico $ toPico $ Sec $ i 17)
              toMilli (Milli $ i 170) `shouldBe` (toMilli $ Pico $ toPico $ Milli $ i 170)
              s1 `shouldBe` i3
        where p1 = toPico (Sec i3)::Int
              m1 = toMilli (Pico p1)::Int
              s1 = toSec (Milli m1)::Int
              i3 = 3::Int

doc::IO()
doc = hspec $ do
    describe "doc Convert" $ do
        it "toPicos milli" $ toPico (Milli 1) `shouldBe` 1000000000
        it "toPicos sec" $  toPico (Sec 1) `shouldBe` 1000000000000
        it "toMillis sec" $  toMilli (Sec 1) `shouldBe` 1000
        it "toDiffTime sec" $  toDiffTime (Sec 1) `shouldBe` (secondsToDiffTime 1)
        it "pico2second" $ pico2second (DmyHmp (DmyHm {D.day = 3, D.month = 4, D.year = 2016, D.hour = 10, D.minute = 31}, 21.2589))
                `shouldBe` YmdHms { Y.day = 3, Y.month = 4, Y.year = 2016, Y.hour = 10, Y.minute = 31, Y.second = 21}
                

i::Int -> Int
i = id