{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Patterns
-- Copyright   :  (C) 2013 Jann Mueller
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  j.mueller.11@ucl.ac.uk
-- Stability   :  experimental
-- Time intervals
----------------------------------------------------------------------------
module Data.Time.Patterns(
    -- * Date Patterns
    days,
    mondays,
    tuesdays,
    wednesdays,
    thursdays,
    fridays,
    saturdays,
    sundays,
    weeks,
    -- * Operations on date patterns
    never,
    every,
    shiftBy,
    -- * Queries
    elementOf,
    instancesFrom
    ) where

import Numeric.Interval
import Control.Lens hiding (elementOf, elements)
import Data.Thyme.Calendar (Day, Days, modifiedJulianDay)
import Data.Thyme.Calendar.WeekDate (mondayWeek, _mwDay)
import Data.Time.Patterns.Internal hiding (elementOf, every, never)
import qualified Data.Time.Patterns.Internal as I
import Prelude hiding (cycle, elem, filter)

-- | An event that occurs every day.
days :: DatePattern
days = IntervalSequence{..} where
    nextInterval t = Just (I t (succ t), days)

-- | Every Monday.
mondays :: DatePattern
mondays = filter (isDayOfWeek 1) days

-- | Every Tuesday.
tuesdays :: DatePattern
tuesdays = filter (isDayOfWeek 2) days

-- | Every Wednesday.
wednesdays :: DatePattern
wednesdays = filter (isDayOfWeek 3) days

-- | Every Thursday.
thursdays :: DatePattern
thursdays = filter (isDayOfWeek 4) days

-- | Every Friday.
fridays :: DatePattern
fridays = filter (isDayOfWeek 5) days

-- | Every Saturday.
saturdays :: DatePattern
saturdays = filter (isDayOfWeek 6) days

-- | Every Sunday.
sundays :: DatePattern
sundays = filter (isDayOfWeek 7) days

-- | Weeks, starting on mondays
weeks :: DatePattern
weeks = IntervalSequence $ \d -> let m = lastMonday d in 
    Just (I m $ addDays 7 m, weeks)

-- | Shift all the results by a number of days
shiftBy :: Days -> DatePattern -> DatePattern
shiftBy n sq = mapS (addDays n) sq

-- | Add a number of days to a day
addDays :: Days -> Day -> Day
addDays n d = (d^.modifiedJulianDay + n)^.from modifiedJulianDay

-- | Take every nth occurrence
every :: Int -> DatePattern -> DatePattern
every = I.every

-- | Check if a date is covered by a DatePattern
elementOf :: Day -> DatePattern -> Bool
elementOf = I.elementOf

-- | Get occurrences of an event starting with a given day
instancesFrom :: Day -> DatePattern -> [Day]
instancesFrom = I.elementsFrom

-- | An event that never occurs
never :: DatePattern
never = I.never
-- TO DO: When easter can be implemented using the combinators, the library
-- can be released.
easter :: DatePattern
easter = undefined

-- | Check if a day interval covers exactly a given weekday
--   with Monday = 1, Tuesday = 2, etc.
isDayOfWeek :: Int -> Interval Day -> Bool
isDayOfWeek d i = case (elements i) of
    [dt] -> dt^. mondayWeek . _mwDay == d
    _   -> False

-- | Get the last Monday before or on the date
lastMonday :: Day -> Day
lastMonday d = case (d^.mondayWeek._mwDay) of
    1 -> d
    _ -> lastMonday $ pred d