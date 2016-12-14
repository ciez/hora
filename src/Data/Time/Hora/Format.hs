module Data.Time.Hora.Format where

import Data.Time.Format (formatTime,defaultTimeLocale)
import Data.Time.Clock
import Data.Time.LocalTime as L
import Data.Time.Calendar
import Data.Time.Hora.Type.Time
import Text.Printf


{- | yyyy-mm-dd   UTC   -}
iso::UTCTime -> String
iso t0 = showGregorian $ utctDay t0


{- | yyyy-mm-dd local  -}
iso'::TimeZone -> UTCTime -> Tz String
iso' tz0 utc0 =
    let lt2 = L.utcToLocalTime tz0 utc0
        day2 = localDay lt2
        (y3,m3,d3) = toGregorian day2
    in Tz tz0 $ printf "%04v-%02v-%02v" y3 m3 d3


{- | yyyymmdd   UTC   -}
ymd::UTCTime -> String
ymd utc0 = formatTime defaultTimeLocale "%Y%m%d" utc0 


{- | yyyymmdd  local -}
ymd'::TimeZone -> UTCTime -> Tz String
ymd' tz0 utc0 =
    let lt2 = L.utcToLocalTime tz0 utc0
        day2 = localDay lt2
        (y3,m3,d3) = toGregorian day2
    in Tz tz0 $ printf "%04v%02v%02v" y3 m3 d3
    

formatUTCTime::String  -- ^ format. see 'formatTime' 
    -> UTCTime
        -> String
formatUTCTime format0 = formatTime defaultTimeLocale format0
        