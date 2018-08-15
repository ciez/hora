module Data.Time.Hora.Type 
    (-- * DatePart  
    DatePart(..),
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
    pico::a     -- ^ excludes seconds. Just fraction as Num    
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

{- | This data type closely mimicks UTCTime, has 'Binary' instance

see "Data.Time.Hora.Part" for conversion between 'UTCTime' and 'UTCTimeBin'      -}
data UTCTimeBin = UTCTimeBin {
            modifiedJulianDay::Integer,    -- ^ The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17
            diffTimeAsPicoseconds::Integer   -- ^ the number of picoseconds in a 'DiffTime'
        }
        deriving (Eq, Show, Generic)

instance Binary UTCTimeBin
-- ^ serializeable


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
