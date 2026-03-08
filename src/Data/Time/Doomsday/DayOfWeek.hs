module Data.Time.Doomsday.DayOfWeek (
    DayOfWeek (..),
    daysOfWeek,
) where

import Data.Time.Doomsday.Enum.Util qualified as Enum

data DayOfWeek
    = Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
  deriving (Show, Eq, Ord)

daysOfWeek :: [DayOfWeek]
daysOfWeek = [Sunday .. Saturday]

-- | Days of week are numbered starting from Sunday as 0
--   and iteration repeats forever.
instance Enum DayOfWeek where
  toEnum :: Int -> DayOfWeek
  toEnum m = case m `mod` 7 of
    0 -> Sunday
    1 -> Monday
    2 -> Tuesday
    3 -> Wednesday
    4 -> Thursday
    5 -> Friday
    _ -> Saturday
  fromEnum :: DayOfWeek -> Int
  fromEnum = \case
    Sunday -> 0
    Monday -> 1
    Tuesday -> 2
    Wednesday -> 3
    Thursday -> 4
    Friday -> 5
    Saturday -> 6
  enumFromTo :: DayOfWeek -> DayOfWeek -> [DayOfWeek]
  enumFromTo = Enum.enumFromToEq
  enumFromThenTo :: DayOfWeek -> DayOfWeek -> DayOfWeek -> [DayOfWeek]
  enumFromThenTo = Enum.enumFromThenToEqZeroBased

instance Num DayOfWeek where
  (+) :: DayOfWeek -> DayOfWeek -> DayOfWeek
  (+) = Enum.iso2 (+)
  (-) :: DayOfWeek -> DayOfWeek -> DayOfWeek
  (-) = Enum.iso2 (-)
  (*) :: DayOfWeek -> DayOfWeek -> DayOfWeek
  (*) = Enum.iso2 (*)
  abs :: DayOfWeek -> DayOfWeek
  abs = id
  signum :: DayOfWeek -> DayOfWeek
  signum = Enum.iso1 signum
  fromInteger :: Integer -> DayOfWeek
  fromInteger = toEnum . fromInteger . (`mod` 7)

instance Real DayOfWeek where
  toRational = toRational . fromEnum

instance Integral DayOfWeek where
  quotRem :: DayOfWeek -> DayOfWeek -> (DayOfWeek, DayOfWeek)
  quotRem = Enum.iso2P quotRem
  toInteger :: DayOfWeek -> Integer
  toInteger = toInteger . fromEnum
