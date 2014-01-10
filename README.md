This package contains a set of primitives and combinators for event patterns. Example: 

 >> import qualified Prelude as P
 >> let sundays = every 2 sunday
 >> let today = (YearMonthDay 2013 12 01)^.from gregorian
 >> P.take 2 $ instancesFrom today sundays
 [2013-12-08, 2013-12-22]

Hackage: http://hackage.haskell.org/package/time-patterns
License: BSD3 (see LICENSE)
