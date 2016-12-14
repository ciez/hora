module Data.Time.Hora.Type.DatePart 
        (DatePart(..)) where

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
 

instance Ord a => Ord (DatePart a) where
    (<=) a0 b0 = 
        let y1 = (year a0, year b0) 
            m1 = (month a0, month b0) 
            d1 = (day a0, day b0)
            h1 = (hour a0, hour b0) 
            min1 = (minute a0, minute b0) 
            s1 = (second a0, second b0) 
            p1 = (pico a0, pico b0) 
            l1 = [y1,m1,d1,h1,min1,s1,p1]
            f1 (Stop bo1) _ = Stop bo1 
            f1 Continue (a1, b1)  
                | a1 < b1 = Stop True   
                | a1 == b1 = Continue   
                | a1 > b1 = Stop False
            res2 = foldl f1 Continue l1
        in case res2 of 
                Continue -> True
                (Stop b2) -> b2   


data Ord_ = Stop Bool | Continue 