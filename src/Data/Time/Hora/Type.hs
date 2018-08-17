module Data.Time.Hora.Type 
    (-- * DatePart
    DatePart(..),
    -- * DatePartSmall
    DatePartSmall(..),
    julian_day_offset,
    mkDay,
    mkMin,
    ErrorDetail(..),
    -- * UTCTimeBin
    UTCTimeBin(..),
    -- * Tz
    Tz(..),
    Tz'(..),
    -- * TimeSpan 
    TimeSpan(..),
    TwoInt(..)) where

import Data.Binary
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series
import GHC.Generics


{- | serializeable structure for essential Date, Time parts

may also be used to construct 'UTCTime'     

see "Data.Time.Hora.Part" for conversion between 'UTCTime' and 'DatePart'   -}
data DatePart a = DatePart {
    year::a,
    month::a,
    day::a,
    hour::a,
    minute::a,
    second::a,
    pico::a     -- ^ excludes seconds. Just fraction as Integral
    } deriving (Show, Generic)


instance Functor DatePart where
    fmap f0 d0 = d0 {
            day = f0 (day d0),
            month = f0 (month d0),
            year = f0 (year d0),
            hour = f0 (hour d0),
            minute = f0 (minute d0),
            second = f0 (second d0),
            pico = f0 (pico d0)
        }
-- ^ for ease of conversion
        

instance Binary (DatePart Int)  
-- ^ serializeable   

instance Binary (DatePart Integer) 
-- ^ serializeable

instance Binary (DatePart String)  
-- ^ serializeable

deriving instance Eq a => Eq (DatePart a)

instance Ord a => Ord (DatePart a) where
    (<=) a0 b0 =
        let y1 = (year a0, year b0)
            m1 = (month a0, month b0)
            d1 = (day a0, day b0)
            h1 = (hour a0, hour b0)
            min1 = (minute a0, minute b0)
            s1 = (second a0, second b0)
            p1 = (pico a0, pico b0)
            l1 = [y1,m1,d1,h1,min1,s1,p1]
            f1 (Stop bo1) _ = Stop bo1
            f1 Continue (a1, b1)
                | a1 < b1 = Stop True
                | a1 == b1 = Continue
                | a1 > b1 = Stop False
            res2 = foldl f1 Continue l1
        in case res2 of
            Continue -> True
            (Stop b2) -> b2
    

-- private
data Ord_ = Stop Bool | Continue

{- | 'UTCTimeBin' closely mimicks 'UTCTime'.

'UTCTimeBin' has 'Binary' instance. The only purpose of 'UTCTimeBin' is to offer faster conversion from / to 'UTCTime' and more compact  serialization compared with 'DatePart'.

see "Data.Time.Hora.Part" for conversion between 'UTCTime' and 'UTCTimeBin'      -}
data UTCTimeBin = UTCTimeBin {
            modifiedJulianDay::Integer,    -- ^ The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17
            diffTimeAsPicoseconds::Integer   -- ^ 'DiffTime' expressed as picoseconds
        }
        deriving (Eq, Show, Generic)

instance Binary UTCTimeBin
-- ^ serializeable


{- |  'DatePartSmall' uses fixed-size storage. Storage (as encoded with "Data.Binary".encode) varies with the constructor used, is noted as \".. bytes\" against each constructor.

      allows to operate with dates only ..

      .. or time (minute / millisecond precision) only

      is convenient for dealing with intervals / timespans

      day count begins at 1 Jan 0001: 1 Jan 0001 is day 1

      max date is: 11759222-01-19. That's 19 Jan 11759222

      see "Data.Time.Hora.Part" for conversion between 'UTCTime' and 'DatePartSmall'

      only values constructed with 'DatePartSmall' can be converted to 'UTCTime'
  -}
data DatePartSmall = Day Word32  {- ^ days after 31 Dec 1 BC: 1 Jan AD 1 is day 1. See https://en.wikipedia.org/wiki/Anno_Domini

   5 bytes     -}
               | Min Word16      {- ^ minutes (includes hours)

   3 bytes
                              -}
               | Ms Word32       {- ^ milliseconds (includes seconds)

5 bytes
               -}
               | Time Word16 Word32  {- ^ minutes, milliseconds

   7 bytes
               -}
               | DatePartSmall Word32 Word16 Word32  {- ^ date, minutes, milliseconds

               11 bytes
               -}
               | Day' Word32     {- ^ date span in days

               5 bytes
               -}
               | Min' Word16     {- ^ time span in minutes

               3 bytes
               -}
               | Ms' Word32      {- ^ time span in milliseconds

               5 bytes
               -}
               | Error ErrorDetail  -- ^ result of failed operation
              deriving (Eq, Show, Generic)

{- |  Julian day offset

https://en.wikipedia.org/wiki/Julian_day     -}
julian_day_offset::Integral a => a
julian_day_offset = fromIntegral 678576


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


mkMin::(Num a, Integral a) =>
        a      -- ^ hour
        -> a   -- ^ minute
        -> DatePartSmall   -- ^ 'Min'
mkMin h0 m0 = Min $ fromIntegral $ h0 * 60 + m0


data ErrorDetail = Invalid    -- ^ operation is not possible with these constructors
                | Overflow    -- ^ data type maxed out
                | Invalid_Overflow  -- ^ 'Invalid' <> 'Overflow'
              deriving (Eq, Show, Generic)

instance Binary ErrorDetail
instance Binary DatePartSmall

instance Semigroup DatePartSmall where
-- combine / merge
   (<>) (Day d0) (Time m0 ms0) = DatePartSmall d0 m0 ms0    -- 1
   (<>) (Min m0) (Ms ms0) = Time m0 ms0                     -- 2
-- increment
   (<>) (DatePartSmall d0 m0 ms0) (Day' d1) = DatePartSmall (d0 + d1) m0 ms0  -- 3 todo overflow
   (<>) (DatePartSmall d0 m0 ms0) (Min' m1) = DatePartSmall d0 (m0 + m1) ms0  -- 4 todo overflow
   (<>) (DatePartSmall d0 m0 ms0) (Ms' ms1) = DatePartSmall d0 m0 $ ms0 + ms1 -- 5 todo overflow
   (<>) (Time m0 ms0) (Min' m1) = Time (m0 + m1) ms0  -- 6 todo overflow
   (<>) (Time m0 ms0) (Ms' ms1) = Time m0 $ ms0 + ms1 -- 7 todo overflow
   (<>) (Day m0) (Day' m1) = Day $ m0 + m1         -- 8 todo overflow
   (<>) (Min m0) (Min' m1) = Min $ m0 + m1         -- 9 todo overflow
   (<>) (Ms m0) (Ms' m1) = Ms $ m0 + m1            -- 10 todo overflow
-- overwrite
   (<>) (DatePartSmall _ _ _) (DatePartSmall d1 m1 ms1) = DatePartSmall d1 m1 ms1      -- 11
   (<>) (Time _ _) (Time m1 ms1) = Time m1 ms1     -- 12
   (<>) (Day _) (Day d1) = Day d1                  -- 13
   (<>) (Min _) (Min m1) = Min m1                  -- 14
   (<>) (Ms _) (Ms ms1) = Ms ms1                   -- 15
-- update
   (<>) (DatePartSmall d0 _ _) (Time m1 ms1) = DatePartSmall d0 m1 ms1  -- 16
   (<>) (Time _ ms0) (Min m1) = Time m1 ms0        -- 17
   (<>) (Time m0 _) (Ms ms1) = Time m0 ms1         -- 18
-- errors
   (<>) (Error Invalid) (Error Invalid) = Error Invalid              -- 19
   (<>) (Error Invalid) (Error Overflow) = Error Invalid_Overflow    -- 20
   (<>) (Error Overflow) (Error Overflow) = Error Overflow           -- 21
   (<>) (Error Overflow) (Error Invalid) = Error Invalid_Overflow    -- 22
   (<>) (Error Invalid_Overflow) _ = Error Invalid_Overflow          -- 23
   (<>) _ (Error Invalid_Overflow) = Error Invalid_Overflow          -- 24
   (<>) e0@(Error _) _ = e0                                          -- 25
   (<>) _ e0@(Error _) = e0                                          -- 26
   (<>) _ _ = Error Invalid                                          -- 27


{- ^ '<>' can be used both to combine parts (e.g. 'Day', 'Time') and
   to add date/time span to the existing parts

   combining parts:

   'Day' <> 'Time' -> 'DatePartSmall'

   'Min' <> 'Ms' -> 'Time'

   adding span:

   'Day' <> 'Day''  -> 'Day'

   'Min' <> 'Min''  -> 'Min'

   'Ms' <> 'Ms''    -> 'Ms'


-}


{-| 'Tz' ('DatePart' a)  parts show local date & time
    
for conversions between timezones see "Data.Time.Hora.Zone"     -}
data Tz a = Tz TimeZone a  deriving (Show,Functor)

deriving instance Eq a => Eq (Tz a) 
deriving instance Ord a => Ord (Tz a) 


-- | 'TimeZone' | 'TimeZoneSeries'
class Tz' tz where
    tz'::tz -> UTCTime -> TimeZone

instance Tz' TimeZone where
    tz' tz0 _ = tz0

instance Tz' TimeZoneSeries where
    tz' = timeZoneFromSeries
{- ^ see "Data.Time.Hora.Zone" re: 'TimeZoneSeries'

use of 'TimeZoneSeries' is preferred when converting from 'UTCTime' to 'DatePart' -}


{- | second and fractions

see "Data.Time.Hora.Span" for conversion    -}
data TimeSpan a = Sec a
                | Pico a
                | Milli a deriving (Show, Functor)


-- | constraint
type TwoInt a b = (Integral a, Integral b)


{- | ! fromInteger returns 'Pico'. assumes the value is Pico seconds

>>> Milli 397100 + (Sec 2) + 37891470000
Pico 399137891470000
>>> Milli 397100 + (Sec 2) + (Pico 37891470000)
Pico 399137891470000
>>> 3 * (Sec 10) == (Sec 30)
True  
>>> 3 * (Pico 10) == (Pico 30)
True        
>>> 300 * (Milli 1000) == (Milli 300000)
True    -}
instance Integral a => Num (TimeSpan a) where
    (+) = withPico (+)
    (*) = withPico (*)
    abs a0 = if a0 > Pico 0 then a0 else - a0
    signum a0
        | a0 > Pico 0 = 1
        | a0 < Pico 0 = - 1
        | otherwise = 0
    fromInteger i0 = Pico $ fromIntegral i0
    (-) = withPico (-)


{- | >>> Sec 1 == Milli 1000
True    -}
instance (Eq a, Integral a) => Eq (TimeSpan a) where
    (==) (Sec a0) (Sec b0) = a0 == b0
    (==) (Milli a0) (Milli b0) = a0 == b0
    (==) (Pico a0) (Pico b0) = a0 == b0
    (==) a0 b0 = a1 == b1
        where a1 = toPico a0::Integer
              b1 = toPico b0::Integer


{- | >>> Sec 1 > Milli 500
True        -}
instance (Ord a, Integral a) => Ord (TimeSpan a) where
    (<=) (Sec a0) (Sec b0) = a0 <= b0
    (<=) (Milli a0) (Milli b0) = a0 <= b0
    (<=) (Pico a0) (Pico b0) = a0 <= b0
    (<=) a0 b0 = a1 <= b1
        where a1 = toPico a0::Integer
              b1 = toPico b0::Integer


withPico::Integral a => (a -> a -> a) ->
    TimeSpan a -> TimeSpan a -> TimeSpan a
withPico fn0 a0 b0 = Pico $ fn0 a1 b1
        where a1 = toPico a0
              b1 = toPico b0


{- | >>> toPico (Milli 1) 
    1000000000 -}
toPico::TwoInt a b => TimeSpan a -> b
toPico (Pico i0) = fromIntegral i0
toPico (Milli i0) = fromIntegral $ i0 * picoMs
toPico (Sec i0) = fromIntegral $ i0 * picoSec



-- | pico in 1 second
picoSec::Integral a => a
picoSec = 1000000000000     --  12

-- | pico in 1 milli
picoMs::Integral a => a
picoMs = 1000000000         --  9               
