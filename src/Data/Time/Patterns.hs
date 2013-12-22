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
    every
    ) where

import Data.Time.Calendar (
    Day, 
    addDays, 
    addGregorianMonthsClip,
    addGregorianYearsClip,
    fromGregorian,
    isLeapYear,
    toGregorian)
import Data.Time.Calendar.WeekDate (
    toWeekDate)

-- | A pattern describing re-occuring events. 
newtype DatePattern = DatePattern { nOcc :: Day -> Maybe (Day, DatePattern) }

-- | The next occurence after a date, or Nothing if no such
--   value exists.
nextOccurrence :: DatePattern -> Day -> Maybe Day
nextOccurrence DatePattern{..} d = nOcc d >>= return . fst

-- | A list of occurrences of the pattern after a start daye.
occurrences :: DatePattern -> Day -> [Day]
occurrences d@DatePattern{..} startDate = theList where
    theList = case (nOcc startDate) of
        Nothing -> []
        Just (a, dp') -> a : occurrences dp' a

-- | A weekly event
weekly :: DatePattern
weekly = DatePattern{..} where
  nOcc d = Just (addDays 7 d, weekly)

-- | A monthly event. 
monthly :: DatePattern
monthly = DatePattern{..} where
  nOcc d = Just (addGregorianMonthsClip 1 d, monthly)

-- | A yearly event.
yearly :: DatePattern
yearly = DatePattern{..} where
  nOcc d = Just (addGregorianYearsClip 1 d, yearly)

-- | An event that occurs only in leap years.
leapYearly :: DatePattern
leapYearly = DatePattern $ \dt ->
    let (y,month,day) = toGregorian dt in
    let nextLeapYear = head . filter isLeapYear . zipWith (+) [1..] . repeat in
    Just (fromGregorian (nextLeapYear y) month day, leapYearly)

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
