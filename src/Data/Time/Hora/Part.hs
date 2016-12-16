-- | convert  between 'UTCTime' and 'DatePart'
module Data.Time.Hora.Part 
        (fromUtc,fromUtc',
        ToUTC(..)) where

import Data.Time.Hora.Type
import Data.Time.Hora.Span
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime as L
import Data.Fixed


{- | UTC -}
fromUtc::Integral a => UTCTime -> DatePart a
fromUtc t0 = 
    let day1 = utctDay t0::Day
        dt1 = utctDayTime t0::DiffTime
        (y1,m1,d1) = toGregorian day1
        tod1 = timeToTimeOfDay dt1::TimeOfDay
        pico4 = todSec tod1::Fixed E12
        (sec5, MkFixed pico5) = properFraction pico4
    in DatePart {
            year = fi y1,
            month = fi m1,
            day = fi d1,
            hour = fi $ todHour tod1,
            minute = fi $ todMin tod1,
            second = fi sec5,
            pico = fi pico5
            }


{- | specified time zone

Tz (DatePart a)  parts show local date & time  

see also "Data.Time.Hora.Zone"  -}
fromUtc'::(Tz' tz, Integral a) => 
    tz -> UTCTime -> Tz (DatePart a)
fromUtc' tz0 utc0 =
    let tz1 = tz' tz0 utc0 
        lt2 = L.utcToLocalTime tz1 utc0
        day2 = localDay lt2
        time2 = localTimeOfDay lt2
        (y3,m3,d3) = toGregorian day2
        d4 = DatePart{
                     year = fi y3,
                     month = fi m3,
                     day = fi d3,
                     hour = fi $ todHour time2,
                     minute = fi $ todMin time2,
                     second = fi sec5,
                     pico = fi pico5
                   }
        pico4 = todSec time2::Fixed E12
        (sec5, MkFixed pico5) = properFraction pico4
    in Tz tz1 d4        


{-| convert 'DatePart' -> 'UTCTime'

Invalid date returns Nothing -}
class ToUTC a where
    toUtc::a -> Maybe UTCTime


instance Integral a => ToUTC (DatePart a) where
    toUtc dp0 =
        let h1 = hour dp0 * 60 * 60 --  as second 
            min1 = minute dp0 * 60  --  as second
            s2 = second dp0 + h1 + min1
            diff1 = secondsToDiffTime $ fi s2
            diff2 = picosecondsToDiffTime $ fi $ pico dp0          
            
            mday1 = fromGregorianValid (fi $ year dp0)
                            (fi $ month dp0) $ fi $ day dp0
        in mday1 >>= \day1 -> Just $ UTCTime day1 $ diff1 + diff2
-- ^ assumes DatePart is UTC 


instance Integral a => ToUTC (Tz (DatePart a)) where
    toUtc (Tz tz0 dp0) =
        let s1 = toPico $ Sec $ second dp0
            mtod1 = makeTimeOfDayValid (fi $ hour dp0) 
                                        (fi $ minute dp0) 
                                        (timeSpanPico $ Pico s1 + (Pico $ fi $ pico dp0)) 
            mday1 = fromGregorianValid (fi $ year dp0)
                            (fi $ month dp0) $ fi $ day dp0
        in mday1 >>= \day1 ->
            mtod1 >>= \tod1 ->
                let lt1 = LocalTime day1 tod1
                    zt1 = ZonedTime lt1 tz0
                in Just $ zonedTimeToUTC zt1         


fi::TwoInt a b => a -> b
fi = fromIntegral                