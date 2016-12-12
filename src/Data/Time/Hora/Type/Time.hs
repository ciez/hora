module Data.Time.Hora.Type.Time where

import Data.Time.LocalTime


data TimeSpan a = Sec a
    | Pico a
    | Milli a deriving (Show, Eq, Ord)


data Tz a = Tz TimeZone a  deriving Show


-- | constraint
type TwoInt a b = (Integral a, Integral b)
