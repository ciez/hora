module Data.Time.Hora.Future where

import Data.Time.Clock
import Data.Time.Hora.Type.Time
import Data.Time.Hora.Convert


{- | 'getCurrentTime' +/- offset

from unit test:

@
getCurrentTime
futureUTCTime $ Milli 100
futureUTCTime $ Sec 3

2016-12-12 15:34:03.138798524 UTC
2016-12-12 15:34:03.23893359 UTC
2016-12-12 15:34:06.138978355 UTC
@   -}
futureUTCTime::Integral a => TimeSpan a -> IO UTCTime
futureUTCTime ts0 = getCurrentTime >>=
                pure . (addUTCTime diff1)
    where diff1 = nominalDiff ts0


-- | time diff in milli
timeDiffMs::UTCTime -> UTCTime -> Int
timeDiffMs t1 t2 =
    let t1_ = utctDayTime t1
        t2_ = utctDayTime t2
        d3 = diffTimeToPicoseconds $ t2_ - t1_
    in toMilli $ Pico d3
                