module Data.Time.Doomsday.Month (
    Month (..),
    allMonths,
    monthLength,
) where

data Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
  deriving (Show, Eq, Ord)

allMonths :: [Month]
allMonths = [January .. December]

monthLength :: Num a => Bool -> Month -> a
monthLength isLeap = \case
  January -> 31
  February -> if isLeap then 29 else 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31

-- | Months are numbered starting from 1 and iteration repeats forever.
instance Enum Month where
  toEnum :: Int -> Month
  toEnum m = case (m - 1 `mod` 12) + 1 of
    1 -> January
    2 -> February
    3 -> March
    4 -> April
    5 -> May
    6 -> June
    7 -> July
    8 -> August
    9 -> September
    10 -> October
    11 -> November
    _ -> December
  fromEnum :: Month -> Int
  fromEnum = \case
    January -> 1
    February -> 2
    March -> 3
    April -> 4
    May -> 5
    June -> 6
    July -> 7
    August -> 8
    September -> 9
    October -> 10
    November -> 11
    December -> 12
  enumFrom :: Month -> [Month]
  enumFrom = map toEnum . enumFrom . fromEnum
  enumFromThen :: Month -> Month -> [Month]
  enumFromThen x y = map toEnum $ enumFromThen (fromEnum x) (fromEnum y)
  enumFromTo :: Month -> Month -> [Month]
  enumFromTo x y = map toEnum $ enumFromTo (fromEnum x) (fromEnum y)
  enumFromThenTo :: Month -> Month -> Month -> [Month]
  enumFromThenTo x y z = map toEnum $ enumFromThenTo (fromEnum x) (fromEnum y) (fromEnum z)
