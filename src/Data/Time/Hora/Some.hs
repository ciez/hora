{-| common format shortcuts -}
module Data.Time.Hora.Some where

import Data.Maybe
import Data.Time.Hora.Format
import Data.Time.Hora.Internal.DatePartSmall as S
import Data.Time.Hora.Internal.Pad
import Data.Time.Hora.Part


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


{- | pretty print 'DatePartSmall'

incremental values are prefixed with + or -
-}
show'::DatePartSmall -> String
show' dp0
   | (Day d0) <- dp0
         = format iso $ fromJust $ toUtc $ DatePartSmall d0 0 0
   | (Day' d0) <- dp0
         = "+" <> (show d0) <> " days"
   | (S.Min m0) <- dp0
         = let hr1 = m0 `div` 60
               min1 = m0 `rem` 60
           in pad1 (show hr1) <> ":" <> pad1 (show min1)
   | (S.Min' m0) <- dp0
         = "+" <> (show' $ S.Min m0)
   | (S.Ms ms0) <- dp0
         = let sec1 = ms0 `div` 1000
               ms1 = ms0 `rem` 1000
           in pad1 (show sec1) <> "." <> (pad '0' 3 $ show ms1)
   | (S.Ms' ms0) <- dp0
         = "+" <> (show' $ S.Ms ms0)
   | (S.Time m0 ms0) <- dp0
        = show' (S.Min m0) <> ":" <> show' (S.Ms ms0)
   | (S.DatePartSmall d0 m0 ms0) <- dp0
        = show' (S.Day d0) <> " " <> show' (S.Min m0) <> ":" <> show' (S.Ms ms0)
   | (S.Neg dp0) <- dp0
        = let _:s1 = show' dp0
          in "-" <> s1
   | otherwise = show dp0
   where pad1 = pad '0' 2
