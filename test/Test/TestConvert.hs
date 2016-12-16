module Test.TestConvert where

import Test.Hspec
import Data.Time.Hora.Type
import Data.Time.Hora.Span
import Data.Time.Clock


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
       describe "TimeSpan Num" $ do
          it "pico Num" $ do 
              Milli 1 + (Milli 2) `shouldBe` (Pico $ (picoMs * 1) + (picoMs * 2))  
              Sec 2 - (Sec 1) `shouldBe` (Pico $ (picoSec * 2) - (picoSec * 1))  
              Pico 2 * (Milli 10) `shouldBe` (Pico $ 2 * (picoMs * 10))
       describe "TimeSpan Ord" $ do
          it "pico Ord" $ do
              Milli 10 `shouldSatisfy` (< (Sec 1))   
              Milli 10 `shouldSatisfy` (> (Milli 9))   
              Pico 10 `shouldSatisfy` (< (Sec 1))   
              Pico 10 `shouldSatisfy` (< (Milli 1))   
              picoMs * 10 `shouldSatisfy` (== (toPico $ Milli 10))   
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
                

i::Int -> Int
i = id