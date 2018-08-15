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


-- | pico in 1 second
picoSec::Integral a => a
picoSec = 1000000000000     --  12

-- | pico in 1 milli
picoMs::Integral a => a
picoMs = 1000000000         --  9

-- | milli in 1 sec
msSec::Integral a => a
msSec = 1000

{- | >>> toPico $ Milli 3
3000000000  -}
toPico::TwoInt a b => TimeSpan a -> b
toPico (Pico i0) = fromIntegral i0
toPico (Milli i0) = fromIntegral $ i0 * picoMs
toPico (Sec i0) = fromIntegral $ i0 * picoSec

{- | >>> toMilli $ Sec 5
5000   -}
toMilli::TwoInt a b => TimeSpan a -> b
toMilli (Pico i0) = fromIntegral $ i0 `div` picoMs
toMilli (Milli i0) = fromIntegral $ i0
toMilli (Sec i0) = fromIntegral $ i0 * msSec

{- | >>> toSec $ Milli 781200
781     -}
toSec::TwoInt a b => TimeSpan a -> b
toSec (Pico i0) = fromIntegral $ i0 `div` picoSec
toSec (Milli i0) = fromIntegral $ i0 `div` msSec
toSec (Sec i0) = fromIntegral $ i0


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
