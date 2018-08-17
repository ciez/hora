module Data.Time.Hora.Span 
    (-- ** 'TimeSpan'
    toPico,
    toMilli,
    toSec,
    -- ** multipliers
    picoSec,
    picoMs,
    msSec,
    -- ** 'Pico' - 'TimeSpan' conversion
    picoTimeSpan,
    timeSpanPico,
    -- ** diff time
    toDiffTime,
    nominalDiff
    ) where

import Data.Fixed
import Data.Ratio
import Data.Time.Clock
import Data.Time.Hora.Type
import Data.Time.Hora.Internal.Span


toDiffTime::Integral a => TimeSpan a -> DiffTime
toDiffTime (Sec s0) = secondsToDiffTime $ fromIntegral s0
toDiffTime (Pico s0) = picosecondsToDiffTime $ fromIntegral s0
toDiffTime t0@(Milli s0) = picosecondsToDiffTime $ toPico t0


nominalDiff::Integral a => TimeSpan a -> NominalDiffTime
nominalDiff ts0 = let s1 = toPico ts0::Integer
                in fromRational $ s1 % picoSec


picoTimeSpan::Integral a => Pico -> TimeSpan a
picoTimeSpan (MkFixed p0) = Pico $ fromIntegral p0


timeSpanPico::Integral a => TimeSpan a -> Pico 
timeSpanPico ts0 = MkFixed $ fromIntegral p0
        where p0 = toPico ts0
