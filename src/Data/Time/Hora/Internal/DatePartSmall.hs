module Data.Time.Hora.Internal.DatePartSmall where

import Data.Binary
import GHC.Generics
import Prelude hiding (negate)


{- |  'DatePartSmall' uses fixed-size storage. Storage (as encoded with "Data.Binary".encode) varies with the constructor used, is noted as \".. bytes\" against each constructor.

      allows to operate with dates only ..

      .. or time (minute / millisecond precision) only

      is convenient for dealing with intervals / timespans

      day count begins at 1 Jan 0001: 1 Jan 0001 is day 1

      max date is: 11759222-01-19. That's 19 Jan 11759222

      see "Data.Time.Hora.Part" for conversion between 'UTCTime' and 'DatePartSmall'

      only values constructed with 'DatePartSmall' can be converted to 'UTCTime'
  -}
data DatePartSmall = Day Word32  {- ^ days after 31 Dec 1 BC: 1 Jan AD 1 is day 1. See https://en.wikipedia.org/wiki/Anno_Domini

   5 bytes     -}
               | Min Word16      {- ^ minutes (includes hours)

   3 bytes
                              -}
               | Ms Word32       {- ^ milliseconds (includes seconds)

5 bytes
               -}
               | Time Word16 Word32  {- ^ minutes, milliseconds

   7 bytes
               -}
               | DatePartSmall Word32 Word16 Word32  {- ^ date, minutes, milliseconds

               11 bytes
               -}
               | Day' Word32     {- ^ date span in days

               5 bytes
               -}
               | Min' Word16     {- ^ time span in minutes

               3 bytes
               -}
               | Ms' Word32      {- ^ time span in milliseconds

               5 bytes
               -}
               | Neg DatePartSmall  -- ^ negate. Hidden constructor
               | Error ErrorDetail  -- ^ result of failed operation
              deriving (Eq, Show, Generic)


{- | 'Day' -> 'Day''

   'Min' -> 'Min''

   'Ms' -> 'Ms''
-}
toSpan::DatePartSmall -> DatePartSmall
toSpan dp0 = case dp0 of
               Day d1 -> Day' d1
               Min m1 -> Min' m1
               Ms ms1 -> Ms' ms1
               otherwise -> dp0

{- | adds hidden Neg constructor to 'Day'', 'Min'' or 'Ms''
to enable negative spans   -}
negate::DatePartSmall -> DatePartSmall
negate dp0 = case dp0 of
               Neg dp1 -> dp1
               Day' d1 -> Neg dp0
               Min' m1 -> Neg dp0
               Ms' ms1 -> Neg dp0
               otherwise -> dp0


data ErrorDetail = Invalid    -- ^ operation is not possible with these constructors
                | Overflow    -- ^ data type maxed out
                | Invalid_Overflow  -- ^ 'Invalid' <> 'Overflow'
              deriving (Eq, Show, Generic)

instance Binary ErrorDetail
instance Binary DatePartSmall

instance Semigroup DatePartSmall where
-- combine / merge
   (<>) (Day d0) (Time m0 ms0) = DatePartSmall d0 m0 ms0    -- 1
   (<>) (Min m0) (Ms ms0) = Time m0 ms0                     -- 2

-- increment
   (<>) (DatePartSmall d0 m0 ms0) (Day' d1) = DatePartSmall (d0 + d1) m0 ms0  -- 3 todo overflow
   (<>) (DatePartSmall d0 m0 ms0) (Min' m1) = DatePartSmall d0 (m0 + m1) ms0  -- 4 todo overflow
   (<>) (DatePartSmall d0 m0 ms0) (Ms' ms1) = DatePartSmall d0 m0 $ ms0 + ms1 -- 5 todo overflow
   (<>) (Time m0 ms0) (Min' m1) = Time (m0 + m1) ms0  -- 6 todo overflow
   (<>) (Time m0 ms0) (Ms' ms1) = Time m0 $ ms0 + ms1 -- 7 todo overflow

   (<>) (Day m0) (Day' m1) = Day $ m0 + m1         -- 8 todo overflow
   (<>) (Min m0) (Min' m1) = Min $ m0 + m1         -- 9 todo overflow
   (<>) (Ms m0) (Ms' m1) = Ms $ m0 + m1            -- 10 todo overflow

   (<>) (Day' m0) (Day' m1) = Day' $ m0 + m1         -- 8 todo overflow
   (<>) (Min' m0) (Min' m1) = Min' $ m0 + m1         -- 9 todo overflow
   (<>) (Ms' m0) (Ms' m1) = Ms' $ m0 + m1            -- 10 todo overflow

-- decrement


-- overwrite
   (<>) (DatePartSmall _ _ _) (DatePartSmall d1 m1 ms1) = DatePartSmall d1 m1 ms1      -- 11
   (<>) (Time _ _) (Time m1 ms1) = Time m1 ms1     -- 12
   (<>) (Day _) (Day d1) = Day d1                  -- 13
   (<>) (Min _) (Min m1) = Min m1                  -- 14
   (<>) (Ms _) (Ms ms1) = Ms ms1                   -- 15
-- update
   (<>) (DatePartSmall d0 _ _) (Time m1 ms1) = DatePartSmall d0 m1 ms1  -- 16
   (<>) (Time _ ms0) (Min m1) = Time m1 ms0        -- 17
   (<>) (Time m0 _) (Ms ms1) = Time m0 ms1         -- 18
-- errors
   (<>) (Error Invalid) (Error Invalid) = Error Invalid              -- 19
   (<>) (Error Invalid) (Error Overflow) = Error Invalid_Overflow    -- 20
   (<>) (Error Overflow) (Error Overflow) = Error Overflow           -- 21
   (<>) (Error Overflow) (Error Invalid) = Error Invalid_Overflow    -- 22
   (<>) (Error Invalid_Overflow) _ = Error Invalid_Overflow          -- 23
   (<>) _ (Error Invalid_Overflow) = Error Invalid_Overflow          -- 24
   (<>) e0@(Error _) _ = e0                                          -- 25
   (<>) _ e0@(Error _) = e0                                          -- 26
   (<>) _ _ = Error Invalid                                          -- 27


{- ^ '<>' can be used both to combine parts (e.g. 'Day', 'Time') and
   to add date/time span to the existing parts

   combining parts:

   'Day' <> 'Time' -> 'DatePartSmall'

   'Min' <> 'Ms' -> 'Time'

   adding span:

   'Day' <> 'Day''  -> 'Day'

   'Min' <> 'Min''  -> 'Min'

   'Ms' <> 'Ms''    -> 'Ms'


-}

