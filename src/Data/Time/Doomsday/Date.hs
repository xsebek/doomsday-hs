{-# LANGUAGE OverloadedRecordDot #-}

module Data.Time.Doomsday.Date (
    Date (..),
    daysFromTo,
) where

import Data.Time.Doomsday.Month
import Data.Time.Doomsday.Year

data Date = Date
  { year :: Year
  , month :: Month
  , day :: Int
  }
  deriving (Eq, Ord, Show)

daysFromTo :: Date -> Date -> Int
daysFromTo a b =
  if a > b then negate $ daysFromTo b a
  else if a.year /= b.year then daysFromTo a (Date a.year 12 31) + yearDist + 1 + daysFromTo (Date b.year 01 01) b
  else if a.month /= b.month then monthDist + daysFromTo a (b {month = a.month})
  else b.day - a.day
 where
  yearDist =  sum (yearLength <$> [succ a.year .. pred b.year])
  monthDist = sum $ monthLength (isLeapYear a.year) <$> [a.month .. pred b.month]
