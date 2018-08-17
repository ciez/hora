module Data.Time.Hora.Internal.Span where

{- | second and fractions

see "Data.Time.Hora.Span" for conversion    -}
data TimeSpan a = Sec a
                | Pico a
                | Milli a deriving (Show, Functor)

-- | constraint
type TwoInt a b = (Integral a, Integral b)


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


-- | pico in 1 second
picoSec::Integral a => a
picoSec = 1000000000000     --  12

-- | pico in 1 milli
picoMs::Integral a => a
picoMs = 1000000000         --  9


-- | milli in 1 sec
msSec::Integral a => a
msSec = 1000
