module Data.Time.Hora.Future where

import Data.Time.Clock
import Data.Time.Hora.Type.Time
import Data.Time.Hora.Convert
import Prelude as P
import Data.Fixed


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

{- | Difference between times with pico precision
    
    return TimeSpan for ease of conversion with "Data.Time.Hora.Convert"    -}
class PicoDiff a where
    (-)::a -> a -> TimeSpan Integer


instance PicoDiff UTCTime where
    (-) t2 t1 =
        let t1_ = utctDayTime t1
            t2_ = utctDayTime t2
            d3 = diffTimeToPicoseconds $ t2_ P.- t1_
        in Pico d3
-- ^ assuming both times are in the same date. Day part is discarded 
 

instance PicoDiff Pico where
    (-) (MkFixed t2) (MkFixed t1) = Pico $ t2 P.- t1
-- ^ e.g. Pico part in 'DmyHmp' 