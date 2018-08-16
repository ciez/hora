module Test.TestStorageSize where

import Data.Binary
import qualified Data.ByteString as B (concat,length)
import qualified Data.ByteString.Lazy as L (toChunks)
import Data.Int
import Data.Time.Clock
import Data.Time.Hora.Part
import Data.Time.Hora.Stamp
import Data.Time.Hora.Type
import Debug.Trace
import Prelude as P
import Test.Hspec


main::IO()
main = hspec $ do
   describe "compare binary size" $ do
      it "Min 10" $ do
         let result1 = Min 10
             bin2 = encode result1
         tr1 "Min 10 size is " bin2
         1 `shouldBe` 1
      it "Day 10" $ do
         let result1 = Day 10
             bin2 = encode result1
         tr1 "Day 10 size is " bin2
         1 `shouldBe` 1
      it "Time 1 2" $ do
         let result1 = Time 1 2
             bin2 = encode result1
         tr1 "Time 1 2 size is " bin2
         1 `shouldBe` 1
      it "DatePartSmall 10 2 1" $ do
         let result10 = DatePartSmall 10 2 1
             bin11 = encode result10
         tr1 "DatePartSmall size is " bin11
         1 `shouldBe` 1
      it "DatePartSmall from now" $ do
         now20 <- now::IO UTCTime
         let result21 = fromUtc now20::DatePartSmall
             bin22 = encode result21
         tr1 "DatePartSmall size is " bin22
         1 `shouldBe` 1
      it "Int64" $ do
         let result21 = 10::Int64
             bin22 = encode result21
         tr1 "Int64 size is " bin22
         1 `shouldBe` 1
     where tr1 desc1 bin1 = traceIO $ desc1 <> (show $ B.length $ B.concat $ L.toChunks bin1)


