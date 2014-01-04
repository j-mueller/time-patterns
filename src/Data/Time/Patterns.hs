{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Patterns
-- Copyright   :  (C) 2013 Jann Mueller
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  j.mueller.11@ucl.ac.uk
-- Stability   :  experimental
-- Patterns for re-occurring events. Use the @DatePattern@ type to build up
-- a pattern, and the functions @elementOf@, @instancesFrom@ and 
-- @intervalsFrom@ to evaluate it.
-- Simple example:
--
-- > import Control.Lens
-- > import Data.Thyme.Calendar
-- > import qualified Prelude as P
-- > Module Main where
-- > 
-- > main = do
-- >   -- get the 6th of April for the next ten years
-- >   let april6 = (take 1 $ shiftBy 5 day) `inEach` april
-- >   let today = (YearMonthDay 2013 12 01)^.from gregorian
-- >   print $ P.take 10 $ instancesFrom today april6
-- >
----------------------------------------------------------------------------
module Data.Time.Patterns(
    -- * Date Patterns
    day,
    mondayWeek,
    sundayWeek,
    month,
    year,
    -- ** Months
    january,
    february,
    march,
    april,
    may,
    june,
    july,
    august,
    september,
    october, 
    november,
    december,
    -- ** Days
    monday,
    tuesday,
    wednesday,
    thursday,
    friday,
    saturday,
    sunday,
    -- * Operations on date patterns
    never,
    every,
    shiftBy,
    inEach,
    take,
    skip,
    except,
    intersect,
    -- * Queries
    elementOf,
    instancesFrom,
    intervalsFrom
    ) where

import Numeric.Interval
import Control.Lens hiding (elementOf, elements, contains)
import Data.Thyme.Calendar (Day, Days, Months, YearMonthDay(..), gregorian, modifiedJulianDay, _ymdYear, _ymdMonth, _ymdDay)
import Data.Thyme.Calendar.WeekDate (_mwDay, _swDay)
import qualified Data.Thyme.Calendar.WeekDate as W
import Data.Time.Patterns.Internal hiding (elementOf, every, never, take, skip, except, intersect, occurrencesFrom)
import qualified Data.Time.Patterns.Internal as I
import Prelude hiding (cycle, elem, filter, take)
import qualified Prelude as P

-- | An event that occurs every month.
month :: DatePattern
month = IntervalSequence $ \t -> 
        let m = firstOfMonth t in
        let m' = addMonths 1 m in
        Just (I m m', month) where
        
-- | Every January.
january :: DatePattern
january = filter (isMonthOfYear 1) month

-- | Every February.
february :: DatePattern
february = filter (isMonthOfYear 2) month

-- | Every March.
march :: DatePattern
march = filter (isMonthOfYear 3) month

-- | Every April.
april :: DatePattern
april = filter (isMonthOfYear 4) month

-- | Every May.
may :: DatePattern
may = filter (isMonthOfYear 5) month

-- | Every June.
june :: DatePattern
june = filter (isMonthOfYear 6) month

-- | Every July.
july :: DatePattern
july = filter (isMonthOfYear 7) month

-- | Every August.
august :: DatePattern
august = filter (isMonthOfYear 8) month

-- | Every September.
september :: DatePattern
september = filter (isMonthOfYear 9) month

-- | Every October.
october :: DatePattern
october = filter (isMonthOfYear 10) month

-- | Every November.
november :: DatePattern
november = filter (isMonthOfYear 11) month

-- | Every December.
december :: DatePattern
december = filter (isMonthOfYear 12) month

-- | An event that occurs every day.
day :: DatePattern
day = IntervalSequence{..} where
    nextInterval t = Just (I t (succ t), day)

-- | Every Monday.
monday :: DatePattern
monday = filter (isDayOfWeek 1) day

-- | Every Tuesday.
tuesday :: DatePattern
tuesday = filter (isDayOfWeek 2) day

-- | Every Wednesday.
wednesday :: DatePattern
wednesday = filter (isDayOfWeek 3) day

-- | Every Thursday.
thursday :: DatePattern
thursday = filter (isDayOfWeek 4) day

-- | Every Friday.
friday :: DatePattern
friday = filter (isDayOfWeek 5) day

-- | Every Saturday.
saturday :: DatePattern
saturday = filter (isDayOfWeek 6) day

-- | Every Sunday.
sunday :: DatePattern
sunday = filter (isDayOfWeek 7) day

-- | Weeks, starting on Monday
mondayWeek :: DatePattern
mondayWeek = IntervalSequence $ \d -> let m = lastMonday d in 
    Just (I m $ addDays 7 m, mondayWeek)

-- | Weeks, starting on Sunday.
sundayWeek :: DatePattern
sundayWeek = IntervalSequence $ \d -> let m = lastSunday d in 
    Just (I m $ addDays 7 m, sundayWeek)

-- | Years, starting from Jan. 1
year :: DatePattern
year = IntervalSequence $ \d -> let m = jan1 d in
    Just (I m $ addYears 1 m, year)

-- | The first pattern repeated for each interval of the
--   second pattern. E.g.:
--   
--   > (take 3 $ every 4 monday) `inEach` year
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

-- | Shift all the results by a number of day
shiftBy :: Days -> DatePattern -> DatePattern
shiftBy n sq = mapS (addDays n) sq

-- | Add a number of day to a day
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

-- | Return only occurrences that are present in both patterns
--
-- > let myBirthday = (take 1 day) `inEach` august
-- > let s = intersect myBirthday sunday
--
-- Will return August 1 in years when it falls on a Sunday
intersect :: DatePattern -> DatePattern -> DatePattern
intersect = I.intersect

-- | Get the date intervals described by the pattern, starting
--   from the specified date. 
--   
--   The intervals range from the first
--   day included by the pattern to the first day after it, so
--   a single day @d@ would be described as @(d ... succ d)@ and
--   the interval for a month will go from the 1st of the month
--   to the 1st of the next month.
intervalsFrom :: Day -> DatePattern -> [Interval Day]
intervalsFrom = I.occurrencesFrom

-- | Check if a day interval covers exactly a given weekday
--   with Monday = 1, Tuesday = 2, etc.
isDayOfWeek :: Int -> Interval Day -> Bool
isDayOfWeek d i = case (elements i) of
    [dt] -> dt^. W.mondayWeek . _mwDay == d
    _   -> False

isMonthOfYear :: Int -> Interval Day -> Bool
isMonthOfYear m = all (isMonth' m) . elements 
    where
        isMonth' i d = d^.gregorian^._ymdMonth == i

-- | Get the last Monday before or on the date
lastMonday :: Day -> Day
lastMonday d = case (d^.W.mondayWeek._mwDay) of
    1 -> d
    _ -> lastMonday $ pred d


-- | Get the last Monday before or on the date
lastSunday :: Day -> Day
lastSunday d = case (d^.W.sundayWeek._swDay) of
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
    let (years,months) = (d'^._ymdMonth + m) `divMod` 12 in
    (YearMonthDay (d'^._ymdYear + years) months (d'^._ymdDay))^.from gregorian

firstOfMonth :: Day -> Day
firstOfMonth d = let d' = d^.gregorian in 
    (YearMonthDay (d'^._ymdYear) (d'^._ymdMonth) 1)^.from gregorian
