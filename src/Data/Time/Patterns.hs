{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Patterns
-- Copyright   :  (C) 2013 Jann Mueller
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  j.mueller.11@ucl.ac.uk
-- Stability   :  experimental
-- Primitives and combinators for describing dates that follow a pattern
-- such as yearly, monthly, and so on.
----------------------------------------------------------------------------
module Data.Time.Patterns(
    -- * Date patterns
    DatePattern,
    nextOccurrence,
    occurrences,
    -- * Primitives
    leapYearly,
    monthly,
    never,
    weekly,
    yearly,
    -- * Combinators
    count,
    every
    ) where

import Control.Lens
import Data.Thyme.Calendar (
    YearMonthDay,
    gregorian,
    gregorianMonthsClip,
    gregorianYearsClip,
    isLeapYear,
    _ymdYear)

-- | A pattern describing re-occuring events. 
newtype DatePattern = DatePattern { nOcc :: YearMonthDay -> Maybe (YearMonthDay, DatePattern) }

-- | The next occurence after a date, or Nothing if no such
--   value exists.
nextOccurrence :: DatePattern -> YearMonthDay -> Maybe YearMonthDay
nextOccurrence DatePattern{..} d = nOcc d >>= return . fst

-- | A list of occurrences of the pattern after a start daye.
occurrences :: DatePattern -> YearMonthDay -> [YearMonthDay]
occurrences DatePattern{..} startDate = theList where
    theList = case (nOcc startDate) of
        Nothing -> []
        Just (a, dp') -> a : occurrences dp' a

-- | A weekly event
weekly :: DatePattern
weekly = DatePattern{..} where
  nOcc d = Just ((head . drop 7 . enumFrom $ d^.from gregorian)^.gregorian, weekly)

-- | A monthly event. 
monthly :: DatePattern
monthly = DatePattern{..} where
  nOcc d = Just (gregorianMonthsClip 1 d, monthly)

-- | A yearly event.
yearly :: DatePattern
yearly = DatePattern{..} where
  nOcc d = Just (gregorianYearsClip 1 d, yearly)

-- | An event that occurs only in leap years.
leapYearly :: DatePattern
leapYearly = DatePattern $ \dt ->
    let nextLeapYear = head . filter (\ty -> isLeapYear (ty^._ymdYear)) . zipWith (gregorianYearsClip) (enumFrom 1) . repeat in
    Just (nextLeapYear dt, leapYearly)

-- | A pattern with no occurrences.
never :: DatePattern
never = DatePattern $ const $ Nothing

-- | Every n-th occurence of an event. If a negative number is given, 
--   getOccurence of the result will always return Nothing.
every :: Int -> DatePattern -> DatePattern
every n DatePattern{..} 
  | n < 0 = never
  | otherwise = DatePattern $ nextOcc n
      where
        nextOcc 0 d = nOcc d
        nextOcc 1 d = nOcc d
        nextOcc n' d = nOcc d >>= nextOcc (n'-1) . fst

-- | Stop after a number of occurrences.
count :: Int -> DatePattern -> DatePattern
count n p@DatePattern{..}
  | n < 1 = never
  | otherwise = DatePattern $ \d -> 
        let nd = nOcc d >>= return . fst in
        let dp = count (n - 1) p in
        nd >>= \d' -> return (d', dp)
        
        