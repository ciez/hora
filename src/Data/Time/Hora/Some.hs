{-| common format shortcuts -}
module Data.Time.Hora.Some where

import Data.Time.Hora.Format


{- | yyyy-mm-dd  -}
iso::[Format]
iso = [Y_m_d]


{- | yyyymmdd   -}
ymd::[Format]
ymd = [Y, Mth_1_12, D]


{- | time.fraction no trailing 0 - varying length

09:10:58.030311306  -}
tf::[Format]
tf = [Hms, Fraction]


{- | time 

09:11:18        -}
t::[Format]
t = [Hms]



{- | date, time 

2016-12-14 09:16:23       -}
dt::[Format]
dt = [Y_m_d, Raw " ", Hms]
