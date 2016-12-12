module Data.Time.Hora.Timestamp 
    (-- ** numerical
    now,
    -- ** string
    t,
    t',
    dt,
    d) where

import Data.Time.Clock
import Data.Time.Hora.WithTimeZone
import Data.Time.Hora.Format
import Data.Time.Hora.Type.DmyHm
import Data.Time.Hora.Type.Time
import Data.Time.Hora.Parse


{-| time with Pico in current timezone 

>>> now
Tz CET (DmyHm {day = 12, month = 12, year = 2016, hour = 19, minute = 33},15.546562180000)
-}
now::IO (Tz DmyHmp)
now = withTimeZone parse'



{- | timestamp 

second.fraction. __UTC__

>>> t
14:41:29.785834836
-}
t::IO String
t = do
      utc0 <- getCurrentTime
      pure $ formatUTCTime hmsFraction utc0


{- | timestamp 

second.fraction. __UTC__

>>> t'
14:41:29 .785834836
-}
t'::IO String
t' = do
      utc0 <- getCurrentTime
      pure $ formatUTCTime hmsFraction' utc0


{- | date, time (to second)  __UTC__

>>> dt
2016-12-12 14:43:13
-}
dt::IO String
dt = do
      utc0 <- getCurrentTime
      pure $ formatUTCTime ymdHms utc0




{- | date stamp /yyyymmdd/   

__local__ timezone

>>> d
Tz CET "20161212"
-}
d::IO (Tz String)
d = withTimeZone ymd

