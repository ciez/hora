module Data.Time.Hora.Parse where

import Data.Time.Hora.Type.DatePart as M
import Data.Time.Hora.Type.Time
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime as L
import Data.Fixed


{- | UTC -}
parse::Num a => UTCTime -> DatePart a
parse t0 = 
    let day1 = utctDay t0::Day
        dt1 = utctDayTime t0::DiffTime
        (y1,m1,d1) = toGregorian day1
        tod1 = timeToTimeOfDay dt1::TimeOfDay
        pico4 = todSec tod1::Fixed E12
        (sec5, MkFixed pico5) = properFraction pico4
    in DatePart {
            year = fromIntegral y1,
            month = fromIntegral m1,
            day = fromIntegral d1,
            hour = fromIntegral $ todHour tod1,
            minute = fromIntegral $ todMin tod1,
            second = fromIntegral sec5,
            pico = fromIntegral pico5
            }


{- | specified time zone -}
parse'::Num a => 
    TimeZone -> UTCTime -> Tz (DatePart a)
parse' tz0 utc0 =
    let lt2 = L.utcToLocalTime tz0 utc0
        day2 = localDay lt2
        time2 = localTimeOfDay lt2
        (y3,m3,d3) = toGregorian day2
        d4 = DatePart{
                     year = fromIntegral y3,
                     month = fromIntegral m3,
                     day = fromIntegral d3,
                     hour = fromIntegral $ todHour time2,
                     minute = fromIntegral $ todMin time2,
                     second = fromIntegral sec5,
                     pico = fromIntegral pico5
                   }
        pico4 = todSec time2::Fixed E12
        (sec5, MkFixed pico5) = properFraction pico4
    in Tz tz0 d4        