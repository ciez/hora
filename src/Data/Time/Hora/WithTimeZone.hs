module Data.Time.Hora.WithTimeZone where

import Data.Time.Clock
import Data.Time.LocalTime as L
import Data.Time.Hora.Type.Time


type WithTimeZone a = TimeZone -> UTCTime -> Tz a

{- | do calc with current time zone from 'getCurrentTimeZone'

    probably don't need it
-}
withTimeZone::WithTimeZone a -> IO (Tz a)
withTimeZone fn0 = do
    z1 <- getCurrentTimeZone    --  CET | CEST
    t1 <- getCurrentTime
    pure $ fn0 z1 t1
