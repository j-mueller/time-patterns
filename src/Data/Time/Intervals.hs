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
module Data.Time.Intervals(
    -- * Types
    IntervalSequence(..),
    DatePattern,
    TimePattern,
    -- * Operations on interval sequences
    never,
    every,
    filter,
    intersect,
    minus, 
    cycle,
    elementOf,
    occurrencesFrom,
    -- * Date Patterns
    days,
    mondays,
    sundays,
    -- * Conversions
    toTimePattern
    ) where

import Numeric.Interval
import Control.Lens hiding (elementOf, elements)
import Data.AdditiveGroup
import Data.Maybe (listToMaybe)
import Data.Thyme.Calendar (Day)
import Data.Thyme.Clock (UTCTime, UTCView(..), _utctDay, utcTime)
import Data.Thyme.Calendar.WeekDate (mondayWeek, _mwDay)
import Prelude hiding (cycle, elem, filter)

-- | If the argument to nextOccurrence is part of an interval, then the result should be the interval containing it.
-- The interval should be closed at the first parameter and open at the second, so that repeated calls of
-- nextOccurrence yield a sequence of occurrences.
newtype IntervalSequence t = IntervalSequence { nextInterval :: t -> Maybe (Interval t, IntervalSequence t)}

type DatePattern = IntervalSequence Day
type TimePattern = IntervalSequence UTCTime -- TimePattern should cycle throu

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


-- | Every monday.
mondays :: DatePattern
mondays = filter (isDayOfWeek 1) days

-- | Every sunday.
sundays :: DatePattern
sundays = filter (isDayOfWeek 7) days

-- | A sequence with no occurrences
never :: IntervalSequence t
never = IntervalSequence $ const $ Nothing

-- | Take every nth occurrence
every :: Int -> IntervalSequence t -> IntervalSequence t
every n sq 
  | n < 1 = never
  | otherwise = IntervalSequence $ nextOcc n
      where
        nextOcc n' d = listToMaybe $ drop (n'-1) $ occurrencesFrom d sq >>= \s -> return (s, every n' sq)

-- | Accept results which satisfy a condition
filter :: (Interval t -> Bool) -> IntervalSequence t -> IntervalSequence t
filter f IntervalSequence{..} = IntervalSequence nOcc' where
    nOcc' t = nextInterval t >>= checkCondition
    checkCondition (p,q) = case (f p) of
        True -> Just (p, filter f q)
        False -> nOcc' $ sup p

intersect :: Num t => IntervalSequence t -> IntervalSequence t -> IntervalSequence t
intersect = undefined

minus :: Num t => IntervalSequence t -> IntervalSequence t -> IntervalSequence t 
minus = undefined

-- | Repeat a point infinitely
cycle :: Interval t -> IntervalSequence t
cycle i = IntervalSequence $ const $ Just (i, cycle i)

-- | Check if a point is covered by an interval sequence
elementOf :: Ord t => t -> IntervalSequence t -> Bool
elementOf t IntervalSequence{..} = maybe False (\(p,_) -> (elem t p) && (<) t (sup p)) (nextInterval t)

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

elements :: Enum a => Interval a -> [a]
elements i = enumFromTo (inf i) (pred $ sup i)

-- | Check if a day interval covers exactly a given weekday
--   with Monday = 1, Tuesday = 2, etc.
isDayOfWeek :: Int -> Interval Day -> Bool
isDayOfWeek d i = case (elements i) of
    [dt] -> dt^. mondayWeek . _mwDay == d
    _   -> False