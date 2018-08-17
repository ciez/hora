-- | convert  between 'UTCTime', 'UTCTimeBin', 'DatePart', 'DatePartSmall'
module Data.Time.Hora.Part 
        (-- * FromUTC
        FromUTC(..),
        fromUtc',
        -- * ToUTC
        ToUTC(..),
        -- * 'DatePartSmall'
        mkDay,
        mkMin,
        mkMs,
        julian_day_offset
         ) where

import Data.Ratio
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Hora.Span
import Data.Time.Hora.Type
import Data.Time.LocalTime as L
import Data.Word

class FromUTC a where
     fromUtc::UTCTime -> a

-- | returns DatePart a in UTC timezone
instance Integral a => FromUTC (DatePart a) where
   fromUtc::Integral a => UTCTime -> DatePart a
   fromUtc t0 =
       let day1 = utctDay t0::Day
           dt1 = utctDayTime t0::DiffTime
           (y1,m1,d1) = toGregorian day1
           (tod1, sec5, pico5) = todSecPico dt1
       in DatePart {
               year = fi y1,
               month = fi m1,
               day = fi d1,
               hour = fi $ todHour tod1,
               minute = fi $ todMin tod1,
               second = fi sec5,
               pico = fi pico5
               }


-- | extract (TimeIfDay, seconds, picoseconds) from 'DiffTime'
todSecPico::Integral a => DiffTime -> (TimeOfDay, a, Integer)
todSecPico dt0 = (tod1, sec5, pico5)
   where tod1 = timeToTimeOfDay dt0::TimeOfDay
         pico4 = todSec tod1::Fixed E12
         (sec5, MkFixed pico5) = properFraction pico4


diffTime::Int        -- ^ hour
        -> Int       -- ^ minute
        -> Fixed E12   -- ^ pico
        -> DiffTime
diffTime h0 m0 p0 = timeOfDayToTime tod1
      where tod1 = TimeOfDay h0 m0 p0


instance FromUTC UTCTimeBin where
      fromUtc::UTCTime -> UTCTimeBin
      fromUtc t0 = UTCTimeBin day1 pico1
            where day1 = toModifiedJulianDay $ utctDay t0
                  pico1 =  diffTimeToPicoseconds $ utctDayTime t0


instance FromUTC DatePartSmall where
      fromUtc::UTCTime -> DatePartSmall
      fromUtc t0 = DatePartSmall day2 minute2 milli2
            where dp1 = fromUtc t0::DatePart Int
                  UTCTimeBin julian1 _ = fromUtc t0::UTCTimeBin
                  day2 = fi julian1 + julian_day_offset
                  minute2 = fi $ hour dp1 * 60 + minute dp1
                  milli2 = fromSec3 + fromPico3
                  fromSec3 = toMilli (Sec $ second dp1)::Word32
                  fromPico3 = toMilli $ Pico $ pico dp1::Word32




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


instance ToUTC UTCTimeBin where
     toUtc (UTCTimeBin day0 pico0) = Just $ UTCTime day1 diff1
         where day1 = ModifiedJulianDay day0
               diff1 = picosecondsToDiffTime pico0


instance ToUTC DatePartSmall where
   toUtc (DatePartSmall d0 m0 ms0) = Just utc1
        where utc1 = UTCTime day2 diff2
              day1 = fi d0 - julian_day_offset::Integer
              day2 = ModifiedJulianDay day1
              diff2 = diffTime hr1 min1 pico2
              min1 = fi $ m0 `rem` 60
              hr1 = fi $ m0 `div` 60
              sec1 = fi $ ms0 `div` 1000
              sec2 = MkFixed $ sec1 * picoSec::Fixed E12
              ms1 = fi ms0::Integer
              pico1 = fromRational $ (ms1 `rem` 1000) % 1000::Fixed E12
              pico2 = sec2 + pico1
   toUtc _ = Nothing


{- |  Julian day offset

https://en.wikipedia.org/wiki/Julian_day     -}
julian_day_offset::Integral a => a
julian_day_offset = fromIntegral 678576


-- | day / date
mkDay::Integral a =>
         a     -- ^ year
        -> a   -- ^ month
        -> a   -- ^ day
        -> DatePartSmall   -- ^ 'Day'
mkDay y0 m0 d0 = maybe (Error Invalid) id mday2
   where mday2 = valid2 <$> mday1::Maybe DatePartSmall
         mday1 = fromGregorianValid y1 m1 d1
         valid2 = Day . fromIntegral . (+ julian_day_offset) . toModifiedJulianDay
         y1 = fromIntegral y0
         m1 = fromIntegral m0
         d1 = fromIntegral d0


-- | minutes including hours
mkMin::(Num a, Integral a) =>
        a      -- ^ hour
        -> a   -- ^ minute
        -> DatePartSmall   -- ^ 'Min'
mkMin h0 m0 = Min $ fromIntegral $ h0 * 60 + m0


-- | milliseconds including seconds
mkMs::(Num a, Integral a) =>
        a      -- ^ second
        -> a   -- ^ millisecond
        -> DatePartSmall   -- ^ 'Ms'
mkMs s0 ms0 = Ms $ fromIntegral $ toMilli (Sec s0) + ms0


fi::TwoInt a b => a -> b
fi = fromIntegral                
