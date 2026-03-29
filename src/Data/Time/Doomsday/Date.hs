{-# LANGUAGE OverloadedRecordDot #-}

module Data.Time.Doomsday.Date (
    Date (..),
    daysFromTo,
    readIsoDate,
) where

import Data.Time.Doomsday.Month
import Data.Time.Doomsday.Year
import Data.Time.Doomsday.String.Pretty
import Data.Char (isDigit)

data Date = Date
  { year :: Year
  , month :: Month
  , day :: Int
  }
  deriving (Eq, Ord, Show)

readIsoDate :: String -> Either String Date
readIsoDate = \case
  [y1,y2,y3,y4,'-',m1,m2,'-',d1,d2]
    | all isDigit [y1,y2,y3,y4,m1,m2,d1,d2]
    -> Right $ Date (read [y1,y2,y3,y4]) (toEnum $ read [m1,m2]) (read [d1,d2])
  _ -> Left "Expected date in format YYYY-MM-DD"

instance Pretty Date where
  pretty :: Date -> String
  pretty (Date y m d) = unwords [show d, show m, show y]

daysFromTo :: Date -> Date -> Int
daysFromTo a b =
  if a > b then negate $ daysFromTo b a
  else if a.year /= b.year then daysFromTo a (Date a.year 12 31) + yearDist + 1 + daysFromTo (Date b.year 01 01) b
  else if a.month /= b.month then monthDist + daysFromTo a (b {month = a.month})
  else b.day - a.day
 where
  yearDist =  sum (yearLength <$> [succ a.year .. pred b.year])
  monthDist = sum $ monthLength (isLeapYear a.year) <$> [a.month .. pred b.month]
