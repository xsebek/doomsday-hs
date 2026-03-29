module Data.Time.Doomsday.Month (
    Month (..),
    allMonths,
    monthLength,
) where

import Data.Time.Doomsday.Enum.Util (Pred (..))
import Data.Time.Doomsday.Enum.Util qualified as Enum
import Data.Time.Doomsday.String.Pretty (Pretty)

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

mod1to12 :: Integral m => m -> m
mod1to12 m = ((m - 1) `mod` 12) + 1

instance Pretty Month

-- | Months are numbered starting from 1 and iteration repeats forever.
instance Enum Month where
  toEnum :: Int -> Month
  toEnum m = case mod1to12 m of
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
    12 -> December
    x -> error $ show x <> " not in <1,12>"
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
  enumFromTo :: Month -> Month -> [Month]
  enumFromTo = Enum.enumFromToEq
  enumFromThenTo :: Month -> Month -> Month -> [Month]
  enumFromThenTo start next end =
    getPred <$> Enum.enumFromThenToEqZeroBased (Pred start) (Pred next) (Pred end)

instance Num Month where
  (+) :: Month -> Month -> Month
  (+) = error "cannot add Month"
  (*) :: Month -> Month -> Month
  (*) = error "cannot mul Month"
  signum :: Month -> Month
  signum = error "cannot signum Month"
  abs :: Month -> Month
  abs = id
  negate :: Month -> Month
  negate = Enum.iso1 negate
  fromInteger :: Integer -> Month
  fromInteger = toEnum . fromInteger . mod1to12
