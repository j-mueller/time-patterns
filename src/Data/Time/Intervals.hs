{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Intervals
-- Copyright   :  (C) 2013 Jann Mueller
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  j.mueller.11@ucl.ac.uk
-- Stability   :  experimental
-- Time intervals
----------------------------------------------------------------------------
module Data.Time.Intervals where

import Numeric.Interval
import Control.Lens
import Data.AdditiveGroup
import Data.Thyme.Calendar (Day)
import Data.Thyme.Clock (UTCTime, UTCView(..), _utctDay, utcTime)

-- if the argument to nextOccurrence is part of an interval, then the result should be the interval containing it.
-- The interval should be closed at the first parameter and open at the second, so that repeated calls of
-- nextOccurrence yield a sequence of occurrences.
newtype IntervalSequence t = IntervalSequence { nextInterval :: t -> Maybe (Interval t, IntervalSequence t)}
-- instance Functor IntervalSequence
-- instance Foldable IntervalSequence
-- instance Traversable IntervalSequence
type DatePattern = IntervalSequence Day
type TimePattern = IntervalSequence UTCTime -- TimePattern should cycle throu
--type ZonedTimePattern = IntervalSequence ZonedTime


--toTimeZone :: TimeZone -> TimePattern -> ZonedTimePattern

days :: DatePattern
days = IntervalSequence{..} where
    nextInterval t = Just (I t (succ t), days)

toUtcTime :: DatePattern -> TimePattern
toUtcTime dp = IntervalSequence ni where
    ni time = (ni' (day time)) >>= \r -> return (fmap dayToUtcTime $ fst r, toUtcTime $ snd r)
    day t = t ^. _utctDay
    ni' = nextInterval dp

every :: Integer -> DatePattern -> DatePattern 
every = undefined

mondays :: DatePattern
mondays = undefined

intersect :: Num t => IntervalSequence t -> IntervalSequence t -> IntervalSequence t
intersect = undefined

minus :: Num t => IntervalSequence t -> IntervalSequence t -> IntervalSequence t 
minus = undefined

cycle :: t -> IntervalSequence t
cycle = undefined

elementOf :: t -> IntervalSequence t -> Bool
elementOf = undefined

occurrencesFrom :: t -> IntervalSequence t -> [t]
occurrencesFrom = undefined

-- TO DO: When easter can be implemented using the combinators, the library
-- can be released.
easter :: DatePattern
easter = undefined

-- Some helper functions

dayToUtcTime :: Day -> UTCTime
dayToUtcTime day = (UTCTime day midnight)^.from utcTime
    where
        midnight = zeroV