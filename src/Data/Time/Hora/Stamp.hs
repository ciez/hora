module Data.Time.Hora.Stamp 
    (-- * numerical
    Now(..),
    -- * 'String'
    Timestamp(..),
    ts') where

import Data.Time.Clock
import Data.Time.Hora.Format
import Data.Time.Hora.Type
import Data.Time.Hora.Part
import Data.Time.LocalTime as L


-- | numeric 
class Now a where
    now::IO a

instance Integral a => Now (DatePart a) where
    now = withUTCTime fromUtc
{-^ UTC 

>>> now::IO(DatePart Int)
DatePart {year = 2016, month = 12, day = 15, hour = 10, minute = 20, second = 31, pico = 494880242000}
-}


instance Now UTCTime where
    now = getCurrentTime
{- ^ >>> now::IO UTCTime
2016-12-15 10:20:54.664155598 UTC       -}


instance Integral a => Now (Tz (DatePart a)) where
    now = withTimeZone fromUtc'
{- ^ local timezone 

'Tz' ('DatePart' a)  parts show local date & time

>>> now::IO(Tz (DatePart Int))
Tz CET (DatePart {year = 2016, month = 12, day = 15, hour = 11, minute = 21, second = 21, pico = 657029375000})     -}



withTimeZone::(TimeZone -> UTCTime -> Tz a) -> IO (Tz a)
withTimeZone fn0 = do
    z1 <- getCurrentTimeZone    --  CET | CEST
    t1 <- getCurrentTime
    pure $ fn0 z1 t1


withUTCTime::(UTCTime -> a) -> IO a
withUTCTime fn0 = do
    t1 <- getCurrentTime
    pure $ fn0 t1


{- | timestamp in specified format

see "Data.Time.Hora.Some" for common ['Format']s      -}
class Timestamp out where
    ts::[Format] -> IO out
    
    
instance Timestamp String where
    ts lf0 = withUTCTime $ format lf0
-- ^ UTC

instance Timestamp (Tz String) where
    ts lf0 = withTimeZone $ format' lf0
-- ^ local timezone


-- | timestamp in specified format, 'TimeZone' | 'TimeZoneSeries'
ts'::Tz' tz => [Format] -> tz -> IO (Tz String)
ts' lf0 tz0 = withUTCTime $ format' lf0 tz0 
        