module Data.Time.Hora.Type.DatePart where

import GHC.Generics
import Data.Binary


-- | Date, Time parts
data DatePart a = DatePart {
    year::a,
    month::a,
    day::a,
    hour::a,
    minute::a,
    second::a,
    pico::a     -- ^ excludes seconds. Just fraction as Num    
    } deriving (Show, Eq, Generic)


instance Functor DatePart where
    fmap f0 d0 = d0 {
            day = f0 (day d0),
            month = f0 (month d0),
            year = f0 (year d0),
            hour = f0 (hour d0),
            minute = f0 (minute d0),
            second = f0 (second d0),
            pico = f0 (pico d0)
        } 

instance Binary (DatePart Int) 
instance Binary (DatePart String)
 

instance Ord (DatePart Int) where
    (<=) a0 b0 = not $  
        year a0 > (year b0)
        || month a0 > (month b0)
        || day a0 > (day b0)
        || hour a0 > (hour b0)
        || minute a0 > (minute b0)
        || second a0 > (second b0)
        || pico a0 > (pico b0)
         
            