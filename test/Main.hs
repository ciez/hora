module Main where

import qualified Test.TestConvert as Conv
import qualified Test.TestDiffTime as T
import qualified Test.TestDmyHm as D
import qualified Test.TestDatePart as Dp
import qualified Test.TestUtc as U
import qualified Test.TestFuture as Fut
import qualified Test.TestPico as P
import qualified Test.TestPico2 as P2
import qualified Test.TestSome as Tcom
import qualified Test.TestFormat as Tf
import qualified Test.TestTime as Tt
import qualified Test.TestUTCTimeBin as Bin
import qualified Test.TestZone as Z
import qualified Test.TestAncientDate as A
import qualified Test.TestDatePartSmall as Sm
import qualified Test.TestStorageSize as Sto
import qualified Test.TestDatePartSmallConvert as Smc
import qualified Test.TestRoundtripPicoMilli as Pm
import qualified Test.TestDatePartSmallDoc as Smd


main::IO()
main = do
        T.main
        Conv.main
        Fut.main
        Tt.main
        P.main
        D.main
        P2.main
        Dp.main
        U.main
        Z.main
        Tcom.main
        Tf.main
        Bin.main
        A.main
        Sm.main
        Sto.main
        Smc.main
        Pm.main
        Smd.main
