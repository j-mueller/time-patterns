{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Monoid (Monoid(..))
import Data.Profunctor (Profunctor(..))
import Prelude hiding (cycle, elem, filter, take)

-- | If the argument to nextOccurrence is part of an interval, then the result should be the interval containing it.
-- The interval should be closed at the first parameter and open at the second, so that repeated calls of
-- nextOccurrence yield a sequence of occurrences.
newtype IntervalSequence t s = IntervalSequence { nextInterval :: t -> Maybe (Interval s, IntervalSequence t s)}

type IntervalSequence' t = IntervalSequence t t

instance (Ord s) => Monoid (IntervalSequence t s) where
    mappend = union
    mempty  = never

instance Functor (IntervalSequence t) where
    fmap f s = IntervalSequence $ \d -> nextInterval s d >>= \r -> return (fmap f $ fst r, fmap f $ snd r)

instance Profunctor IntervalSequence where
    rmap = fmap
    lmap f s = IntervalSequence $ \d -> (nextInterval s $ f d) >>= \r -> return (fst r, lmap f $ snd r)

-- | A sequence with no occurrences
never :: IntervalSequence t s
never = IntervalSequence $ const $ Nothing

-- | Take every nth occurrence
every :: Int -> IntervalSequence' t -> IntervalSequence' t
every n sq@IntervalSequence{..} 
  | n < 1 = never
  | otherwise = IntervalSequence $ nextOcc 1
      where
        nextOcc n'  d 
            | n' == n = nextInterval d >>= \s -> return (fst s, every n sq)
            | otherwise = nextInterval d >>= nextOcc (n' + 1) . sup . fst

-- | Accept results which satisfy a condition
filter :: (Interval t -> Bool) -> IntervalSequence' t -> IntervalSequence' t
filter f IntervalSequence{..} = IntervalSequence nOcc' where
    nOcc' t = nextInterval t >>= checkCondition
    checkCondition (p,q) = case (f p) of
        True -> Just (p, filter f q)
        False -> nOcc' $ sup p


-- | Repeat a point infinitely
cycle :: Interval s -> IntervalSequence t s
cycle i = IntervalSequence $ const $ Just (i, cycle i)

-- | Check if a point is covered by an interval sequence
elementOf :: Ord t => t -> IntervalSequence' t -> Bool
elementOf t IntervalSequence{..} = maybe False (\(p,_) -> (elem t p) && (<) t (sup p)) (nextInterval t)

-- | The sequence of occurrences from an initial point.
occurrencesFrom :: t -> IntervalSequence' t -> [Interval t]
occurrencesFrom start IntervalSequence{..} = case (nextInterval start) of
    Nothing -> []
    Just (res, sq') -> res : occurrencesFrom (sup res) sq'

-- | Elements covered by an interval sequence from an initial point.
elementsFrom :: Enum t => t -> IntervalSequence' t -> [t]
elementsFrom start sq = concat $ fmap elements $ occurrencesFrom start sq

elements :: Enum a => Interval a -> [a]
elements i = enumFromTo (inf i) (pred $ sup i)

-- | End a sequence after n occurrences
take :: Int -> IntervalSequence t s -> IntervalSequence t s
take n IntervalSequence{..} 
    | n < 1     = never
    | otherwise = IntervalSequence $ \d -> 
        nextInterval d >>= \r -> Just (fst r, take (pred n) $ snd r)

-- | Skip the first n occurrences of a sequence
skip :: Int -> IntervalSequence' t -> IntervalSequence' t
skip n sq
  | n < 0 = never
  | otherwise = IntervalSequence $ nextOcc (nextInterval sq) n
      where
        nextOcc ni n' d 
            | n' < 1 = ni d 
            | otherwise = ni d >>= \(p, q) -> nextOcc (nextInterval q) (n' - 1) (sup p)

-- | Take occurrences until an interval is reached
stopAt :: Ord s => Interval s -> IntervalSequence t s -> IntervalSequence t s
stopAt p IntervalSequence{..} = IntervalSequence ni' where
    ni' d = nextInterval d >>= \(p', q) -> case (p' `contains` p) of
        True -> Nothing
        False -> return (p', stopAt p q)

stopAt' :: Ord s => s -> IntervalSequence t s -> IntervalSequence t s
stopAt' p IntervalSequence{..} = IntervalSequence ni' where
    ni' d = nextInterval d >>= \(p', q) -> case (sup p' >= p) of
        True -> Nothing
        False -> return (p', stopAt' p q)

-- | Stop as soon as a result greater than or equal to the parameter
--   is produced
before :: Ord s => Interval s -> IntervalSequence t s -> IntervalSequence t s
before p IntervalSequence{..} = IntervalSequence ni' where
    ni' d = nextInterval d >>= \(p', q) -> case (p >=! p') of
        False -> Nothing
        True  -> return (p', stopAt p q)

skipUntil :: Ord t => Interval t -> IntervalSequence' t -> IntervalSequence' t
skipUntil fr sq = IntervalSequence $ nextOcc $ nextInterval sq where
    nextOcc ni d = ni d >>= \(p', q) -> case (fr <=! p') of
        False -> nextOcc (nextInterval q) (sup p')
        True  -> return (p', q)

-- | Skip over a point in the sequence. All occurrences of this
--   datum are removed.
except :: (Enum t, Ord t) => t -> IntervalSequence' t -> IntervalSequence' t
except p = except' (p ... succ p)

-- | Skip over all intervals which contain the parameter
except' :: Ord t => Interval t -> IntervalSequence' t -> IntervalSequence' t
except' p IntervalSequence{..} = IntervalSequence ni' where
    ni' d = nextInterval d >>= \(p', q) -> case (p' `contains` p) of
        False -> return (p', except' p q) 
        True -> ni' $ sup p

firstOccurrenceIn :: (Enum t, Ord t) => t -> Interval t -> IntervalSequence' t -> Maybe (Interval t, IntervalSequence' t)
firstOccurrenceIn s i IntervalSequence{..} = firstOcc s where
    firstOcc start = do
        (p, q) <- nextInterval start
        case (i `contains` p) of
            True -> return (p, q)
            False -> case (sup p < sup i) of
                True ->  firstOcc $ sup p
                False -> Nothing

-- | Return intervals that are exactly the same
intersect :: (Ord t, Enum t) => IntervalSequence' t -> IntervalSequence' t -> IntervalSequence' t
intersect a b = IntervalSequence (nOcc' a b) where
    nOcc' a' b' d = do
        (ia, sa) <- nextInterval a' d
        (ib, sb) <- nextInterval b'  $ inf ia
        case ((sup ia == sup ib) && (inf ia == inf ib)) of
            True -> return (ib, intersect sa sb)
            False -> nOcc' b' sa $ sup ia -- mix up a' and b' to search in both directions evenly

-- | Merge two sequences into one by switching between them
diag :: IntervalSequence t s -> IntervalSequence t s -> IntervalSequence t s
diag a b = IntervalSequence (nOcc' a b) where
    nOcc' a' b' d = do
        (na, sa) <- nextInterval a' d
        return (na, diag b' sa)

-- | Occurrences from both intervals.
union :: Ord s => IntervalSequence t s -> IntervalSequence t s -> IntervalSequence t s
union a b = IntervalSequence $ \d ->
    case (nextInterval a d, nextInterval b d) of
        (Nothing, Nothing) -> Nothing
        (Nothing, b')       -> b'
        (a',       Nothing) -> a'
        (Just (ia, sa), Just (ib, sb)) -> 
            case (sup ia <= sup ib) of
                True -> return (ia, union sa (ib `andThen` sb))
                False -> return (ib, union (ia `andThen` sa) sb)

-- | Prepend an interval to an interval sequence
andThen :: Interval s -> IntervalSequence t s -> IntervalSequence t s
andThen i sq = IntervalSequence $ \_ -> Just (i, sq)

