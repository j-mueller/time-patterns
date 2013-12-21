{-# LANGUAGE RecordWildCards #-}
module Data.Time.Patterns where

import Data.Time.Calendar (Day, addDays, addGregorianMonthsClip)

-- | A pattern describing re-occuring events
newtype DatePattern = DatePattern { nextOccurence :: Day -> Maybe Day }

-- | A weekly event
weekly :: DatePattern
weekly = DatePattern{..} where
  nextOccurence = Just . addDays 7

-- | A monthly event. 
monthly :: DatePattern
monthly = DatePattern{..} where
  nextOccurence = Just . addGregorianMonthsClip 1


