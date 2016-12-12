module Data.Time.Hora.Type.DmyHm where

import GHC.Generics
import Data.Binary
import Data.Fixed
import qualified Data.Time.Hora.Type.YmdHms as S


-- | Date, Time w/out second
data DmyHm a = DmyHm {
    day::a,
    month::a,
    year::a,
    hour::a,
    minute::a
    } deriving (Show, Eq)


instance Functor DmyHm where
    fmap f0 d0 = d0 {
            day = f0 (day d0),
            month = f0 (month d0),
            year = f0 (year d0),
            hour = f0 (hour d0),
            minute = f0 (minute d0)
        } 


-- | @(DmyHm {day = "12", month = "12", year = "2016", hour = "17", minute = "32"},"59.727280400000")@
type DmyHmp' = (DmyHm String, String)    --  dmyhm


-- | @DmyHmp (DmyHm {day = 12, month = 12, year = 2016, hour = 17, minute = 32},59.727482058000)@
newtype DmyHmp = DmyHmp (DmyHm Int, Pico) deriving (Eq, Show, Generic)  --  dmyhm, pico

deriving instance Generic (DmyHm Int)
instance Binary (DmyHm Int)
instance Binary DmyHmp

{- | convert from more precise to more common type -}
pico2second::DmyHmp -> S.YmdHms
pico2second (DmyHmp (r0,pico0)) = S.YmdHms {
    S.year = year r0,
    S.month = month r0,
    S.day = day r0,
    S.hour = hour r0,
    S.minute = minute r0,
    S.second = round pico0
    }
    

instance Ord DmyHmp where
    (<=) (DmyHmp(a1,p1)) (DmyHmp(a2,p2)) = 
        if a1 <= a2 then p1 <= p2
        else False


instance Ord (DmyHm Int) where
    (<=) a0 b0 = not $  
        year a0 > (year b0)
        || month a0 > (month b0)
        || day a0 > (day b0)
        || hour a0 > (hour b0)
        || minute a0 > (minute b0)
         
            