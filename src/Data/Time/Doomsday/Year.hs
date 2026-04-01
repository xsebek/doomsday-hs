module Data.Time.Doomsday.Year (
  Year,
  isLeapYear,
  yearLength,
) where


type Year = Integer


isLeapYear :: Year -> Bool
isLeapYear y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)


yearLength :: Year -> Int
yearLength y = if isLeapYear y then 366 else 365
