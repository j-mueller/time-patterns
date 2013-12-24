-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Intervals
-- Copyright   :  (C) 2013 Jann Mueller
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  j.mueller.11@ucl.ac.uk
-- Stability   :  experimental
-- Time intervals
----------------------------------------------------------------------------
module Data.Time.Intervals where

import Numeric.Interval
import Data.Thyme.Calendar (YearMonthDay)
import Data.Thyme.Clock (UTCTime)

type DateInterval = Interval YearMonthDay
type TimeInterval = Interval UTCTime