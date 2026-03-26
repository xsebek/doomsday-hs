{-# LANGUAGE OverloadedRecordDot #-}

module Data.Time.Doomsday.Date (
    Date (..),
    daysFromTo,
    readYMD,
) where

import Data.Time.Doomsday.Month
import Data.Time.Doomsday.Year
import Data.Time.Doomsday.String.Pretty
import Text.Read (readMaybe)

data Date = Date
  { year :: Year
  , month :: Month
  , day :: Int
  }
  deriving (Eq, Ord, Show)

readYMD :: String -> Maybe Date
readYMD s = case splitOn '-' s of
  [y, m, d] -> Date <$> readMaybe y <*> fmap toEnum (readMaybe m) <*> readMaybe d
  _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn c = \case
  (a : as)
    | a /= c -> case splitOn c as of
      [] -> [[a]]
      (s:ss) -> ((a:s):ss)
    | otherwise -> [] : splitOn c as
  [] -> [[]]

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
