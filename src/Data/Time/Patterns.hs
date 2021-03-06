{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Patterns
-- Copyright   :  (C) 2013-2017 Jann Müller
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  j.mueller.11@ucl.ac.uk
-- Stability   :  experimental
-- Patterns for recurring events. Use the @DatePattern@ type to build up
-- a pattern, and the functions @elementOf@, @instancesFrom@ and
-- @intervalsFrom@ to evaluate it.
-- Simple example:
--
-- > import Control.Lens
-- > import Data.Time.Calendar (fromGregorian)
-- > import Data.Time.Patterns
-- > import qualified Prelude as P
-- > Module Main where
-- >
-- > main = do
-- >   -- get the 6th of April for the next ten years
-- >   let april6 = (take 1 $ skip 5 day) `inEach` april
-- >   let today =  fromGregorian 2013 12 01
-- >   print $ P.take 10 $ instancesFrom today april6
--
-- @DatePattern@s can be combined using @union@, @intersect@ with their
-- obvious meanings and @inEach@ which repeats one pattern inside another one.
-- For example,
--
-- > ((take 1 day) `inEach` august) `intersect` sunday
--
-- will give the 1st of August in years when it falls on a Sunday.
----------------------------------------------------------------------------
module Data.Time.Patterns(
    -- * Date Patterns
    DatePattern,
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
    union,
    until,
    -- * Queries
    elementOf,
    instancesFrom,
    intervalsFrom
    ) where

import           Data.Time.Calendar             (Day, addDays, fromGregorian,
                                                 toGregorian)
import           Data.Time.Calendar.OrdinalDate (mondayStartWeek,
                                                 sundayStartWeek)
import qualified Data.Time.Calendar.WeekDate    as W
import           Data.Time.Patterns.Internal    hiding (elementOf, every,
                                                 except, intersect, never,
                                                 occurrencesFrom, skip, take,
                                                 union)
import qualified Data.Time.Patterns.Internal    as I
import           Numeric.Interval
import           Prelude                        hiding (cycle, elem, filter,
                                                 take, until)

-- | A `DatePattern` describes a sequence of intervals of type 
-- `Data.Time.Calendar.Day`.
type DatePattern = IntervalSequence' Day

-- | An event that occurs every month.
month :: DatePattern
month = IntervalSequence $ \t ->
        let m = firstOfMonth t in
        let m' = addMonths 1 m in
        Just (m ... m', month) where

-- | Every January.
january :: DatePattern
january = monthOfYear 1

-- | Every February.
february :: DatePattern
february = monthOfYear 2

-- | Every March.
march :: DatePattern
march = monthOfYear 3

-- | Every April.
april :: DatePattern
april = monthOfYear 4

-- | Every May.
may :: DatePattern
may = monthOfYear 5

-- | Every June.
june :: DatePattern
june = monthOfYear 6

-- | Every July.
july :: DatePattern
july = monthOfYear 7

-- | Every August.
august :: DatePattern
august = monthOfYear 8

-- | Every September.
september :: DatePattern
september = monthOfYear 9

-- | Every October.
october :: DatePattern
october = monthOfYear 10

-- | Every November.
november :: DatePattern
november = monthOfYear 11

-- | Every December.
december :: DatePattern
december = monthOfYear 12

-- | An event that occurs every day.
day :: DatePattern
day = IntervalSequence{..} where
    nextInterval t = Just (t ... (succ t), day)

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
    Just (m ... addDays 7 m, mondayWeek)

-- | Weeks, starting on Sunday.
sundayWeek :: DatePattern
sundayWeek = IntervalSequence $ \d -> let m = lastSunday d in
    Just (m ... addDays 7 m, sundayWeek)

-- | Years, starting from Jan. 1
year :: DatePattern
year = IntervalSequence $ \d -> let m = jan1 d in
    Just (m ... addYears 1 m, year)

-- | The first pattern repeated for each interval of the
--   second pattern. E.g.:
--
--   > (take 3 $ every 4 monday) `inEach` year
--
--  will give the fourth, eighth and twelveth Monday in each year
inEach :: DatePattern -> DatePattern -> DatePattern
inEach i o = IntervalSequence (inEach' o i i)

-- | like inEach, except that the ``inner`` DatePattern is replaced by a sequence of DatePatterns
--   so that for every new outer interval, the next element from the sequence will be used.
inEach' :: DatePattern -> DatePattern -> DatePattern -> Day -> Maybe (Interval Day, DatePattern)
inEach' outer inner orig d = do
        (o1, outer') <- nextInterval outer d
        let inner' = stopAt' (sup o1) inner
        case (firstOccurrenceIn (max d $ inf o1) o1 inner') of
                Nothing           -> inEach' outer' orig orig $ sup o1
                Just (i1,inner'') -> return (i1, IntervalSequence $ inEach' outer inner'' orig)

-- | Shift all the results by a number of day
shiftBy :: Integer -> DatePattern -> DatePattern
shiftBy n = mapSequence (addDays n)

-- | Take every nth occurrence
every :: (Num i, Ord i) => i -> DatePattern -> DatePattern
every = I.every

-- | Stop after n occurrences
take :: (Num i, Ord i) => i -> DatePattern -> DatePattern
take = I.take

-- | Skip the first n occurrences
skip :: (Num i, Ord i) => i -> DatePattern -> DatePattern
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

-- | Occurrences of both patterns.
--
-- > union april june
--
-- Will return the months April and June in each year
--
-- > let fifteenth = (take 1 $ skip 14 day) `inEach` month
-- > let third = (take 1 $ skip 2 day) `inEach` month
-- > union fifteenth third
--
-- Will return the 3rd and the 15th of each month
union :: DatePattern -> DatePattern -> DatePattern
union = I.union

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
    [dt] -> let (_, dayOfWeek) = mondayStartWeek dt in dayOfWeek == d
    _   -> False

-- | Get the last Monday before or on the date
lastMonday :: Day -> Day
lastMonday d = let (_, dayOfWeek) = mondayStartWeek d in
    case dayOfWeek of
    1 -> d
    _ -> lastMonday $ pred d


-- | Get the last Sunday before or on the date
lastSunday :: Day -> Day
lastSunday d = let (_, dayOfWeek) = sundayStartWeek d in
    case dayOfWeek of
    1 -> d
    _ -> lastSunday $ pred d

-- | Get the beginning of a year
jan1 :: Day -> Day
jan1 d = let (year, _, _) = toGregorian d in
    fromGregorian year 1 1

addYears :: Integer -> Day -> Day
addYears n d = let (year, month, day) = toGregorian d in
    fromGregorian (year + n) month day

addMonths :: Int -> Day -> Day
addMonths m d =
    let (year, month, day) = toGregorian d in
    let (years,months) = (month + m) `divMod` 12 in
    fromGregorian (year + fromIntegral years) (months) day

firstOfMonth :: Day -> Day
firstOfMonth d =
    let (year, month, _) = toGregorian d in
    fromGregorian year month 1

get1stOfMonth :: Int -> Day -> Day
get1stOfMonth i d =
    let (year, month, day) = toGregorian d in
    let y = abs $ (i - month) `div` 12 in
    fromGregorian (year + fromIntegral y) i 1

getMonth :: Int -> Day -> Interval Day
getMonth i d = (d' ... addMonths 1 d')
    where
        d' = get1stOfMonth i d

monthOfYear :: Int -> DatePattern
monthOfYear i = IntervalSequence $ \d -> Just (getMonth i d, monthOfYear i)

-- | Only include date intervals that end before the given date.
-- 
-- > >> let third = (take 1 $ skip 2 day) `inEach` month
-- > >> let pattern = third `until` (fromGregorian 2020 12 01)
-- > >> instancesFrom (fromGregorian 2017 01 01) pattern
-- > [2017-01-03,2017-02-03 ... 2020-10-03,2020-11-03]
until :: DatePattern -> Day -> DatePattern
until = flip I.stopAt'