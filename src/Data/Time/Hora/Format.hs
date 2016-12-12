module Data.Time.Hora.Format where

import Data.Time.Format (formatTime,defaultTimeLocale)
import Data.Time.Clock
import Data.Time.Hora.Type.DmyHm as M
import Data.Time.LocalTime as L
import qualified Text.Regex.Do.Replace.Template as F
import qualified Text.Regex.Do.Pad as F
import Data.Time.Calendar
import Data.Time.Hora.Type.Time


{- | yyyymmdd      -}
ymd::TimeZone -> UTCTime -> Tz String
ymd tz0 utc0 =
    let lt2 = L.utcToLocalTime tz0 utc0
        day2 = localDay lt2
        (y3,m3,d3) = toGregorian day2
        f4 i4 = F.pad '0' 2 $ show i4
    in Tz tz0 $ "{0}{1}{2}" F.< (f4 <$> [fromIntegral y3,m3,d3])
    

{- | format UTCTime just as you need it. uses 'defaultTimeLocale' 

@
import Text.Regex.Do.Replace.Template 
import Prelude hiding ((\<),(\>))
...

it "formatUTCTime" $ do
    t1 <- getCurrentTime 
    let p1@DmyHm{..} = partFormats
    traceIO $ formatUTCTime (("{day}/{month} {hour}:{minute}") < 
                    [("day",day),("month",month),("hour",hour),("minute",minute)])
                    t1

12/12 18:39                      
@   -}
formatUTCTime::String  -- ^ see 'formatTime'. can also make format with functions in this module 
    -> UTCTime
        -> String
formatUTCTime format0 = formatTime defaultTimeLocale format0


-- | %T%Q
hmsFraction::String
hmsFraction = "%T%Q"

-- | %T %Q
hmsFraction'::String
hmsFraction' = "%T %Q"


-- | %F %T
ymdHms::String
ymdHms = "%F %T"

{- | %S.%-q     

second.pico         -}
spicoFormat::String
spicoFormat = "%S.%-q"  



{- | some formats  /no padding/ 

    see 'formatTime' for full measure

@
day     %-d
month   %-m
year    %Y
hour    %-H
minute  %-M      @   -}
partFormats::DmyHm String
partFormats = DmyHm {
    M.day = "%-d",
    M.month = "%-m",
    M.year = "%Y",
    M.hour = "%-H",
    M.minute = "%-M"
    }
