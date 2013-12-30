{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Intervals
-- Copyright   :  (C) 2013 Jann Mueller
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  j.mueller.11@ucl.ac.uk
-- Stability   :  experimental
-- Time intervals
----------------------------------------------------------------------------
module Data.Time.Intervals(
    -- * Types
    IntervalSequence(..),
    DatePattern,
    TimePattern,
    -- * Operations on interval sequences
    never,
    every,
    intersect,
    minus, 
    cycle,
    elementOf,
    occurrencesFrom,
    -- * Date Patterns
    days,
    mondays,
    -- * Conversions
    toTimePattern
    ) where

import Numeric.Interval
import Control.Lens hiding (elementOf)
import Data.AdditiveGroup
import Data.Maybe (listToMaybe)
import Data.Thyme.Calendar (Day)
import Data.Thyme.Clock (UTCTime, UTCView(..), _utctDay, utcTime)
import Prelude hiding (cycle)

-- if the argument to nextOccurrence is part of an interval, then the result should be the interval containing it.
-- The interval should be closed at the first parameter and open at the second, so that repeated calls of
-- nextOccurrence yield a sequence of occurrences.
newtype IntervalSequence t = IntervalSequence { nextInterval :: t -> Maybe (Interval t, IntervalSequence t)}
{-instance Functor IntervalSequence where
    fmap f IntervalSequence{..} = IntervalSequence ni'
        where
            ni' = nextInterval . f >>= \(a, b) -> return (fmap f a, fmap f b)-}
-- instance Functor IntervalSequence
-- instance Foldable IntervalSequence
-- instance Traversable IntervalSequence
type DatePattern = IntervalSequence Day
type TimePattern = IntervalSequence UTCTime -- TimePattern should cycle throu
--type ZonedTimePattern = IntervalSequence ZonedTime


--toTimeZone :: TimeZone -> TimePattern -> ZonedTimePattern

-- | An event that occurs every day.
days :: DatePattern
days = IntervalSequence{..} where
    nextInterval t = Just (I t (succ t), days)

-- | Convert a DatePattern to a TimePattern
toTimePattern :: DatePattern -> TimePattern
toTimePattern dp = IntervalSequence ni where
    ni time = (ni' (day time)) >>= \r -> return (fmap dayToUtcTime $ fst r, toTimePattern $ snd r)
    day t = t ^. _utctDay
    ni' = nextInterval dp

mondays :: DatePattern
mondays = undefined

-- | A sequence with no occurrences
never :: forall t. IntervalSequence t
never = IntervalSequence $ const $ Nothing

-- | Take every nth occurrence
every :: Int -> IntervalSequence t -> IntervalSequence t
every n sq 
  | n < 1 = never
  | otherwise = IntervalSequence $ nextOcc n
      where
        nextOcc n' d = listToMaybe $ drop (n'-1) $ occurrencesFrom d sq >>= \s -> return (s, every n' sq)

intersect :: Num t => IntervalSequence t -> IntervalSequence t -> IntervalSequence t
intersect = undefined

minus :: Num t => IntervalSequence t -> IntervalSequence t -> IntervalSequence t 
minus = undefined

cycle :: t -> IntervalSequence t
cycle = undefined

elementOf :: t -> IntervalSequence t -> Bool
elementOf = undefined

-- | The sequence of occurrences from an initial point.
occurrencesFrom :: t -> IntervalSequence t -> [Interval t]
occurrencesFrom start IntervalSequence{..} = case (nextInterval start) of
    Nothing -> []
    Just (res, sq') -> res : occurrencesFrom (sup res) sq'

-- TO DO: When easter can be implemented using the combinators, the library
-- can be released.
easter :: DatePattern
easter = undefined

-- Some helper functions

dayToUtcTime :: Day -> UTCTime
dayToUtcTime day = (UTCTime day midnight)^.from utcTime
    where
        midnight = zeroV