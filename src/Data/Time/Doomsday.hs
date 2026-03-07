module Data.Time.Doomsday (
  -- Years
  Year,
  isLeapYear,
) where

type Year = Integer

isLeapYear :: Year -> Bool
isLeapYear y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)
