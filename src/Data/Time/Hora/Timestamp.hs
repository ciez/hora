module Data.Time.Hora.Timestamp 
    (-- ** numerical
    now, now',
    -- ** string
    t, tf,
    dt,
    d, d') where

import Data.Time.Clock
import Data.Time.Hora.Format
import Data.Time.Hora.Type.DatePart
import Data.Time.Hora.Type.Time
import Data.Time.Hora.Parse
import Data.Time.LocalTime as L


{-| UTC 

>>> now
DatePart {year = 2016, month = 12, day = 14, hour = 9, minute = 7, second = 10, pico = 233275605000}
-}
now::Num a => IO (DatePart a)
now = withUTCTime parse


{-| current timezone 

>>> now'
Tz CET (DatePart {year = 2016, month = 12, day = 14, hour = 10, minute = 7, second = 26, pico = 498313115000})
-}
now'::Num a => IO (Tz (DatePart a))
now' = withTimeZone parse'



{- | time.fraction UTC

>>> tf
09:10:58.030311306
-}
tf::IO String
tf = do
      utc0 <- getCurrentTime
      pure $ formatUTCTime "%T%Q" utc0


{- | time UTC

>>> t
09:11:18
-}
t::IO String
t = do
      utc0 <- getCurrentTime
      pure $ formatUTCTime "%T" utc0


{- | date, time  UTC

>>> dt
"2016-12-14 09:16:23"
-}
dt::IO String
dt = do
      utc0 <- getCurrentTime
      pure $ formatUTCTime "%F %T" utc0



{- | date  yyyymmdd   

UTC

>>> d
"20161214"
-}
d::IO String
d = withUTCTime ymd


{- | date yyyymmdd   

local timezone

>>> d'
Tz CET "20161214"
-}
d'::IO (Tz String)
d' = withTimeZone ymd'



type WithLocalTimeZone a = TimeZone -> UTCTime -> Tz a
type WithUTCTime a = UTCTime -> a


{- | do calc with current time zone from 'getCurrentTimeZone'

    probably don't need it
-}
withTimeZone::WithLocalTimeZone a -> IO (Tz a)
withTimeZone fn0 = do
    z1 <- getCurrentTimeZone    --  CET | CEST
    t1 <- getCurrentTime
    pure $ fn0 z1 t1


withUTCTime::WithUTCTime a -> IO a
withUTCTime fn0 = do
    t1 <- getCurrentTime
    pure $ fn0 t1
