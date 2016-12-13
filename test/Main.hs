module Main where

import qualified Test.TestDiffTime as T
import qualified Test.TestConvert as Conv
import qualified Test.TestFuture as Fut
import qualified Test.TestTime as Tt
import qualified Test.TestPico as P
import qualified Test.TestPico2 as P2
import qualified Test.TestDmyHm as D 


main::IO()
main = do
        T.main
        Conv.main
        Fut.main
        Tt.main
        P.main
        D.main
        P2.main
