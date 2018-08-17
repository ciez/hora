module Data.Time.Hora.Format 
    (-- * type
    Format(..),
    -- * format
    format,
    format',
    show') where

import Data.Maybe
import Data.String
import Data.Time.Clock
import Data.Time.Format (formatTime,defaultTimeLocale,FormatTime(..))
import Data.Time.Hora.Type hiding (Min)
import Data.Time.LocalTime as L
import Data.Time.Hora.Part
import qualified Data.Time.Hora.Internal.DatePartSmall as S
import Data.Time.Hora.Internal.Pad


-- | format as UTC
format::[Format] -> UTCTime -> String
format lf0 utc0 = withDefaultLocale (build lf0) utc0


-- | format as local time in specified timezone
format'::Tz' tz =>
    [Format] -> tz -> UTCTime -> Tz String
format' lf0 tz0 utc0 = Tz tz1 $ withDefaultLocale (build lf0) zt1
        where zt1 = utcToZonedTime tz1 utc0::ZonedTime
              tz1 = tz' tz0 utc0


withDefaultLocale::FormatTime t =>
        String  -- ^ format. see 'formatTime' 
        -> t
        -> String
withDefaultLocale = formatTime defaultTimeLocale 


instance IsString Format where
    fromString = Raw 
{- ^ enter 'Raw' 'String' as ordinary 'String' with

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}      
@   -}

{- | \*0: 0 padded

see 'formatTime'
-} 
data Format = 
    Raw String 
    | Tab
    | Crlf    -- ^ new line
    | Offset  -- ^ time zone
    | Zone    -- ^ time zone name
    | Hm      -- ^ %H:%M
    | Hms     -- ^ %H:%M:%S
    | AM      -- ^ AM PM
    | Am     -- ^ am pm
    | H_24    -- ^ 24-hour *0
    | H_12    -- ^ 12-hour *0
    | Min      -- ^ minute *0            
    | S        -- ^ second *0            
    | Fraction_fixed   -- ^ pico precision *0   
    | Fraction   -- ^ .12  for 0.12 second. Pico precision, no trailing 0   
    | Y_m_d    -- ^  %Y-%m-%d
    | Y        -- ^ year
    | Month    -- ^ month name long
    | Mth      -- ^ month name short
    | Mth_1_12    -- ^ month *0
    | D        -- ^ day *0
    | D_wk_1_7   -- ^ day of week for Week Date format, 1 - 7
    | D_wk_0_6   -- ^ day of week number, 0 (= Sunday) - 6 (= Saturday)
    | D_wk      -- ^ day of week short
    | D_week    -- ^ day of week long 
    | Wk_year_Sun -- ^ week of year start Sunday               
    | Wk_year_Mon -- ^ week of year start Monday




{-| build format string from 'Format' -}
class Build f where
    build::f -> String


instance Build [Format] where
    build = concat . (build <$>)
    
    
instance Build Format where    
    build f0 = case f0 of 
        Raw s1 -> s1
        Tab -> "%t"
        Crlf -> "%n"
        Offset -> "%z"
        Zone -> "%Z"
        Hm -> "%R"
        Hms -> "%T"
        AM -> "%p"
        Am -> "%P"
        H_24 -> "%H"
        H_12 -> "%I"
        Min -> "%M"
        S -> "%S"
        Fraction_fixed -> "%q"
        Fraction -> "%Q"
        Y_m_d -> "%F"
        Y -> "%Y"
        Month -> "%B"
        Mth -> "%b"
        Mth_1_12 -> "%m"
        D -> "%d"
        D_wk_1_7 -> "%u"
        D_wk_0_6 -> "%w"
        D_wk -> "%a"
        D_week -> "%A"
        Wk_year_Sun -> "%U"
        Wk_year_Mon -> "%W"



{- | pretty print 'DatePartSmall'

incremental values are prefixed with + or -

>>> show' $ mkDay 2018 08 17
    2018-08-17

>>> show' $ mkMin 4 3
    04:03

>>> show' $ mkMs 7 318
    07.318

>>> let (Day d1) = mkDay 2018 08 17
        (Min m1) = mkMin 15 17
        (Ms ms1) = mkMs 7 358
    show' $ DatePartSmall d1 m1 ms1
    2018-08-17 15:17:07.358

>>>  show' $ Day' 3
     +3

>>> show' $ toSpan $ mkMin 0 53
    +00:53

>>> show' $ toSpan $ mkMs 7 0
    +07.000

>>> show' $ T.negate $ Day' 3
    -3

>>> show' $ T.negate $ toSpan $ mkMin 14 53
    -14:53
-}
show'::DatePartSmall -> String
show' dp0
   | (Day d0) <- dp0
         = format [Y_m_d] $ fromJust $ toUtc $ DatePartSmall d0 0 0
   | (Day' d0) <- dp0
         = "+" <> (show d0)
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
