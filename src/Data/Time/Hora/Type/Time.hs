module Data.Time.Hora.Type.Time 
    (Tz(..),
    TwoInt(..),
    TimeSpan(..)) where

import Data.Time.LocalTime


data Tz a = Tz TimeZone a  deriving (Show,Functor)


-- | various precision
data TimeSpan a = Sec a
    | Pico a
    | Milli a deriving (Show, Functor)


-- | constraint
type TwoInt a b = (Integral a, Integral b)




{- | ! fromInteger returns 'Pico'. assumes the value is Pico seconds

>>> Milli 397100 + (Sec 2) + 37891470000
Pico 399137891470000
-}
instance Integral a => Num (TimeSpan a) where
    (+) = withPico (+)
    (*) = withPico (*)
    abs a0 = if a0 > Pico 0 then a0 else - a0
    signum a0
        | a0 > Pico 0 = 1
        | a0 < Pico 0 = - 1
        | otherwise = 0
    fromInteger i0 = Pico $ fromIntegral i0
    (-) = withPico (-)

{- | safe to mix sec \/ pico \/ milli

>>> Sec 1 == Milli 1000
True
-} 
instance (Eq a, Integral a) => Eq (TimeSpan a) where
    (==) (Sec a0) (Sec b0) = a0 == b0
    (==) (Milli a0) (Milli b0) = a0 == b0
    (==) (Pico a0) (Pico b0) = a0 == b0
    (==) a0 b0 = a1 == b1
        where a1 = toPico a0::Integer
              b1 = toPico b0::Integer


{- | safe to mix sec \/ pico \/ milli

>>> Sec 1 > Milli 500
True        -} 
instance (Ord a, Integral a) => Ord (TimeSpan a) where
    (<=) (Sec a0) (Sec b0) = a0 <= b0
    (<=) (Milli a0) (Milli b0) = a0 <= b0
    (<=) (Pico a0) (Pico b0) = a0 <= b0
    (<=) a0 b0 = a1 <= b1
        where a1 = toPico a0::Integer
              b1 = toPico b0::Integer


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



-- | pico in 1 second
picoSec::Integral a => a
picoSec = 1000000000000     --  12

-- | pico in 1 milli
picoMs::Integral a => a
picoMs = 1000000000         --  9              