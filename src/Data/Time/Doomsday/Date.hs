{-# LANGUAGE OverloadedRecordDot #-}

module Data.Time.Doomsday.Date (
  Date (..),
  daysFromTo,
  readIsoDate,
) where

import Data.Char (isDigit)
import Data.Time.Doomsday.Month
import Data.Time.Doomsday.String.Pretty
import Data.Time.Doomsday.Year


data Date = Date
  { year :: Year
  , month :: Month
  , day :: Int
  }
  deriving (Eq, Ord, Show)


readIsoDate :: String -> Either String Date
readIsoDate = \case
  [y1, y2, y3, y4, '-', m1, m2, '-', d1, d2]
    | all isDigit [y1, y2, y3, y4, m1, m2, d1, d2] ->
        Right $ Date (read [y1, y2, y3, y4]) (toEnum $ read [m1, m2]) (read [d1, d2])
  _ -> Left "Expected date in format YYYY-MM-DD"


instance Pretty Date where
  format (Date y m d) = format d <+> format m <+> format y


daysFromTo :: Date -> Date -> Int
daysFromTo a b
  | a > b = negate $ daysFromTo b a
  | a.year /= b.year = daysFromTo a (Date a.year 12 31) + yearDist + 1 + daysFromTo (Date b.year 01 01) b
  | a.month /= b.month = monthDist + daysFromTo a (b{month = a.month})
  | otherwise = b.day - a.day
 where
  yearDist = sum (yearLength <$> [succ a.year .. pred b.year])
  monthDist = sum $ monthLength (isLeapYear a.year) <$> [a.month .. pred b.month]
