{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Patterns.Internal
-- Copyright   :  (C) 2013 Jann Mueller
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  j.mueller.11@ucl.ac.uk
-- Stability   :  experimental
-- Internal stuff for time patterns
----------------------------------------------------------------------------
module Data.Time.Patterns.Internal where

import Numeric.Interval
import Data.Thyme.Calendar (Day)
import Data.Thyme.Clock (UTCTime)
import Prelude hiding (cycle, elem, filter)

-- | If the argument to nextOccurrence is part of an interval, then the result should be the interval containing it.
-- The interval should be closed at the first parameter and open at the second, so that repeated calls of
-- nextOccurrence yield a sequence of occurrences.
newtype IntervalSequence t = IntervalSequence { nextInterval :: t -> Maybe (Interval t, IntervalSequence t)}

type DatePattern = IntervalSequence Day
type TimePattern = IntervalSequence UTCTime -- TimePattern should cycle throu


-- | A sequence with no occurrences
never :: IntervalSequence t
never = IntervalSequence $ const $ Nothing

-- | Take every nth occurrence
every :: Int -> IntervalSequence t -> IntervalSequence t
every n sq@IntervalSequence{..} 
  | n < 1 = never
  | otherwise = IntervalSequence $ nextOcc 1
      where
        nextOcc n'  d 
            | n' == n = nextInterval d >>= \s -> return (fst s, every n sq)
            | otherwise = nextInterval d >>= nextOcc (n' + 1) . sup . fst

-- | Accept results which satisfy a condition
filter :: (Interval t -> Bool) -> IntervalSequence t -> IntervalSequence t
filter f IntervalSequence{..} = IntervalSequence nOcc' where
    nOcc' t = nextInterval t >>= checkCondition
    checkCondition (p,q) = case (f p) of
        True -> Just (p, filter f q)
        False -> nOcc' $ sup p


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

-- | Elements covered by an interval sequence from an initial point.
elementsFrom :: Enum t => t -> IntervalSequence t -> [t]
elementsFrom start sq = concat $ fmap elements $ occurrencesFrom start sq

elements :: Enum a => Interval a -> [a]
elements i = enumFromTo (inf i) (pred $ sup i)

-- | Apply a function to the results of an interval sequence
mapS :: (t -> t) -> IntervalSequence t -> IntervalSequence t
mapS f IntervalSequence{..} = IntervalSequence nOcc' where
	nOcc' d = nextInterval d >>= \r -> return (fmap f $ fst r, snd r)