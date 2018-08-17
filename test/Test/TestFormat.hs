module Test.TestFormat where

import Data.Time.Hora.Format
import Data.Time.Hora.Part
import Data.Time.Hora.Some
import Data.Time.Hora.Stamp
import Data.Time.Hora.Type as T
import Debug.Trace
import Test.Hspec


main::IO()
main = hspec $ do
       describe "Test.TestFormat" $ do
          it "prefix tab \n Offset" $ do
            ts ["prefix", Tab, "postfix", Crlf, Offset] >>= utc
            1 `shouldBe` 1
          it "zone hm am" $ do
            ts [Zone, " ", Hm, " ", AM, " ", Am, " ", H_12, " ", H_24] >>= utc
            1 `shouldBe` 1
          it "fraction fixed, fraction" $ do
            ts [Fraction, " ", Fraction_fixed] >>= utc
            1 `shouldBe` 1
          it "Months" $ do
            ts [Month, " ", Mth, " ", Mth_1_12] >>= utc
            1 `shouldBe` 1
          it "day of week" $ do
            ts [D_week, " ", D_wk, " ", D_wk_0_6, " ", D_wk_1_7] >>= utc
            1 `shouldBe` 1
          it "week of year" $ do
            ts [Wk_year_Mon, " ", Wk_year_Sun] >>= utc
            1 `shouldBe` 1

       describe "TestFormat show DatePartSmall" $ do
          it "Day" $ do
            show' (mkDay 2018 08 17) `shouldBe` "2018-08-17"
          it "Min" $ do
            show' (mkMin 4 3) `shouldBe` "04:03"
          it "Ms" $ do
            show' (mkMs 7 318) `shouldBe` "07.318"
          it "Time" $ do
            let (T.Min m1) = mkMin 5 7
                (T.Ms ms1) = mkMs 7 58
            show' (T.Time m1 ms1) `shouldBe` "05:07:07.058"
          it "DatePartSmall" $ do
            let (T.Day d1) = mkDay 2018 08 17
                (T.Min m1) = mkMin 15 17
                (T.Ms ms1) = mkMs 7 358
            show' (T.DatePartSmall d1 m1 ms1) `shouldBe` "2018-08-17 15:17:07.358"
          it "Day'" $ do
            show' (Day' 3) `shouldBe` "+3 days"
          it "Min'" $ do
            show' (toSpan $ mkMin 14 53) `shouldBe` "+14:53"
            show' (toSpan $ mkMin 0 53) `shouldBe` "+00:53"
          it "Ms'" $ do
            show' (toSpan $ mkMs 7 358) `shouldBe` "+07.358"
          it "Neg Day'" $ do
            show' (T.negate $ Day' 3) `shouldBe` "-3 days"
          it "Neg Min'" $ do
            show' (T.negate $ toSpan $ mkMin 14 53) `shouldBe` "-14:53"
            show' (T.negate $ toSpan $ mkMin 0 53) `shouldBe` "-00:53"
          it "Neg Ms'" $ do
            show' (T.negate $ toSpan $ mkMs 7 358) `shouldBe` "-07.358"


utc::Tz String -> IO()
utc = traceIO . show                        
