{-| to get timezone-series data, see
<https://hackage.haskell.org/package/timezone-olson timezone-olson> 

see also 
<https://hackage.haskell.org/package/timezone-series timezone-series>

for complete example with olson, see Test.TestZone in this package

@
import Data.Time.LocalTime.TimeZone.Olson

-- see man tzfile for common olson file locations
getTimeZoneSeriesFromOlsonFile "\/usr\/share\/zoneinfo\/Chile\/Continental" >>=
    \\(chile1::TimeZoneSeries) -> ...            
@       -}
module Data.Time.Hora.Zone where

import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Series
import Data.Time.Hora.Part
import Data.Time.Hora.Type


{-| convert __a__ to __b__ in specified 'TimeZone' -}
class ToTimeZone a b where
    toTimeZone::TimeZoneSeries -> a -> b
    
    
instance Integral a => 
            ToTimeZone UTCTime 
                        (Tz (DatePart a)) where
    toTimeZone series0 utc0 = 
        let tz1 = timeZoneFromSeries series0 utc0
        in fromUtc' tz1 utc0
-- ^ similar to 'fromUtc'' but safer around dates when e.g. Summer time changes   


instance Integral a => 
            ToTimeZone (Tz (DatePart a))
                        (Maybe (Tz (DatePart a))) where
    toTimeZone series0 dp0 = toUtc dp0 >>= Just . (toTimeZone series0)
-- ^ convert 'DatePart' from one 'TimeZone' to another        
                