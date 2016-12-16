module Test.TestFormat where

import Test.Hspec
import Debug.Trace
import Data.Time.Hora.Stamp
import Data.Time.Hora.Format
import Data.Time.Hora.Type


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


utc::Tz String -> IO()
utc = traceIO . show                        