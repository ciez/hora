module Data.Time.Hora.Format 
    (-- * type
    Format(..),
    -- * format
    format,
    format') where

import Data.Time.Format (formatTime,defaultTimeLocale,FormatTime(..))
import Data.Time.Clock
import Data.Time.LocalTime as L
import Data.Time.Hora.Type
import Data.String


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
    | Wk_year_Sun -- ^ week of year start sunday               
    | Wk_year_Mon -- ^ week of year start sunday




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
