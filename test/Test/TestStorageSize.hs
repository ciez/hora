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
         size1 bin2 `shouldBe` 3
      it "Day 10" $ do
         let result1 = Day 10
             bin2 = encode result1
         tr1 "Day 10 size is " bin2
         size1 bin2  `shouldBe` 5
      it "Ms 10" $ do
         let result1 = Ms 10
             bin2 = encode result1
         tr1 "Ms 10 size is " bin2
         size1 bin2 `shouldBe` 5
      it "Time 1 2" $ do
         let result1 = Time 1 2
             bin2 = encode result1
         tr1 "Time 1 2 size is " bin2
         size1 bin2 `shouldBe` 7
      it "DatePartSmall 10 2 1" $ do
         let result1 = DatePartSmall 10 2 1
             bin2 = encode result1
         tr1 "DatePartSmall size is " bin2
         size1 bin2 `shouldBe` 11
      it "DatePartSmall from now" $ do
         now20 <- now::IO UTCTime
         let result1 = fromUtc now20::DatePartSmall
             bin1 = encode result1
         tr1 "DatePartSmall now size is " bin1
         size1 bin1 `shouldBe` 11
         let result2 = fromUtc now20::UTCTimeBin
             bin2 = encode result2
         tr1 "UTCTimeBin now size is " bin2
--         size1 bin2 `shouldBe` 22
         let result3 = fromUtc now20::DatePart Int
             bin3 = encode result3
         tr1 "DatePart Int now size is " bin3
--         size1 bin3 `shouldBe` 56
      it "Int64" $ do
         let result1 = 10::Int64
             bin2 = encode result1
         tr1 "Int64 size is " bin2
         size1 bin2 `shouldBe` 8
     where tr1 desc1 bin1 = traceIO $ desc1 <> (show $ size1 bin1)
           size1 bin1 = B.length $ B.concat $ L.toChunks bin1


