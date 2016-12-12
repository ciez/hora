module Data.Time.Hora.Parse where

import Data.Time.Hora.Type.DmyHm as M
import Data.Time.Hora.Type.Time
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime as L
import Data.Time.Hora.Format



{- | specified time in specified zone, to 'DmyHmP' -}
parse'::TimeZone -> UTCTime -> Tz DmyHmp
parse' tz0 utc0 =
    let lt2 = L.utcToLocalTime tz0 utc0
        day2 = localDay lt2
        time2 = localTimeOfDay lt2
        (y3,m3,d3) = toGregorian day2
        d4 = DmyHm{
                     M.year = fromIntegral y3,
                     M.month = m3,
                     M.day = d3,
                     M.hour = todHour time2,
                     M.minute = todMin time2
                   }
        pico4 = todSec time2
    in Tz tz0 $ DmyHmp (d4,pico4)


{- | from specified 'UTCTime' -}
class Parse out where
    parse::UTCTime -> out
    
    
instance Parse DmyHmp where
    parse::UTCTime -> DmyHmp
    parse t0 = let (dm1,pico1) = parse t0
            in DmyHmp (read <$> dm1,read pico1)


instance Parse DmyHmp' where
    parse::UTCTime -> DmyHmp'
    parse t1 =
        let dm1 = flip formatUTCTime t1 <$> partFormats
            pico2 = formatUTCTime spicoFormat t1
        in (dm1,pico2)

