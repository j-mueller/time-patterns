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
    mondayWeeks,
    sundayWeeks,
    months,
    years,
    -- ** Months
    january,
    -- ** Days
    mondays,
    tuesdays,
    wednesdays,
    thursdays,
    fridays,
    saturdays,
    sundays,
    -- * Operations on date patterns
    never,
    every,
    shiftBy,
    inEach,
    take,
    skip,
    except,
    -- * Queries
    elementOf,
    instancesFrom
    ) where

import Numeric.Interval
import Control.Lens hiding (elementOf, elements, contains)
import Data.Thyme.Calendar (Day, Days, Months, YearMonthDay(..), gregorian, modifiedJulianDay, _ymdYear, _ymdMonth, _ymdDay)
import Data.Thyme.Calendar.WeekDate (mondayWeek, sundayWeek, _mwDay, _swDay)
import Data.Time.Patterns.Internal hiding (elementOf, every, never, take, skip, except)
import qualified Data.Time.Patterns.Internal as I
import Prelude hiding (cycle, elem, filter, take)
import qualified Prelude as P

-- | An event that occurs every month.
months :: DatePattern
months = IntervalSequence{..} where
    nextInterval t = 
        let m = firstOfMonth t in
        let m' = addMonths 1 m in
        Just (I m m', months) where
        
-- | Every January.
january :: DatePattern
january = undefined

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

-- | Weeks, starting on Mondays
mondayWeeks :: DatePattern
mondayWeeks = IntervalSequence $ \d -> let m = lastMonday d in 
    Just (I m $ addDays 7 m, mondayWeeks)

-- | Weeks, starting on Sundays.
sundayWeeks :: DatePattern
sundayWeeks = IntervalSequence $ \d -> let m = lastSunday d in 
    Just (I m $ addDays 7 m, sundayWeeks)

-- | Years, starting from Jan. 1
years :: DatePattern
years = IntervalSequence $ \d -> let m = jan1 d in
    Just (I m $ addYears 1 m, years)

-- | The first pattern repeated for each interval of the
--   second pattern. E.g.:
--   
--   > (take 3 $ every 4 mondays) `inEach` years
--
--  will give the fourth, eighth and twelveth Monday in each year
inEach :: DatePattern -> DatePattern -> DatePattern
inEach i o = IntervalSequence (inEach' o (P.repeat i))

-- | like inEach, except that the ``inner`` DatePattern is replaced by a sequence of DatePatterns
--   so that for every new outer interval, the next element from the sequence will be used.
inEach' :: DatePattern -> [DatePattern] -> Day -> Maybe (Interval Day, DatePattern)
inEach' _ [] _ = Nothing
inEach' outer (inner:rest) d = do
        (o1, outer') <- nextInterval outer d
        let inner' = stopAt' (sup o1) inner
        case (firstOccurrenceIn d o1 inner') of
                Nothing           -> inEach' outer' rest $ sup o1
                Just (i1,inner'') -> return (i1, IntervalSequence $ inEach' outer (inner'':rest))

-- | Shift all the results by a number of days
shiftBy :: Days -> DatePattern -> DatePattern
shiftBy n sq = mapS (addDays n) sq

-- | Add a number of days to a day
addDays :: Days -> Day -> Day
addDays n d = (d^.modifiedJulianDay + n)^.from modifiedJulianDay

-- | Take every nth occurrence
every :: Int -> DatePattern -> DatePattern
every = I.every

-- | Stop after n occurrences
take :: Int -> DatePattern -> DatePattern
take = I.take

-- | Skip the first n occurrences
skip :: Int -> DatePattern -> DatePattern
skip = I.skip

-- | Skip over all occurrences of a day.
--   If the pattern describes a  period longer
--   than a day, the entire period will be
--   skipped.
except :: Day -> DatePattern -> DatePattern
except = I.except

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


-- | Get the last Monday before or on the date
lastSunday :: Day -> Day
lastSunday d = case (d^.sundayWeek._swDay) of
    1 -> d
    _ -> lastSunday $ pred d

-- | Get the beginning of a year
jan1 :: Day -> Day
jan1 d = let d' = d^.gregorian in 
    (YearMonthDay (d'^._ymdYear) 1 1)^.from gregorian

addYears :: Int -> Day -> Day
addYears n d = let d' = d^.gregorian in 
    (YearMonthDay (d'^._ymdYear + n) (d'^._ymdMonth) (d'^._ymdDay))^.from gregorian

addMonths :: Months -> Day -> Day
addMonths m d = let d' = d^.gregorian in 
    (YearMonthDay (d'^._ymdYear) (d'^._ymdMonth + m) (d'^._ymdDay))^.from gregorian

firstOfMonth :: Day -> Day
firstOfMonth d = let d' = d^.gregorian in 
    (YearMonthDay (d'^._ymdYear) (d'^._ymdMonth) 1)^.from gregorian
