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

import Data.Time.Hora.Part
import Data.Time.Hora.Type


-- ^ convert 'DatePart' from one 'TimeZone' to another        
toTimeZone::(TwoInt a b, Tz' tz) => 
    tz -- ^ target timezone
    -> (Tz (DatePart a)) -- ^ local (to source timezone) date & time 
        -> (Maybe (Tz (DatePart b))) {- ^ local (to target timezone) date & time
            
                        'Nothing' if source date is invalid-} 
toTimeZone series0 dp0 = toUtc dp0 >>= Just . (fromUtc' series0)
                