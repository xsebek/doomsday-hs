module Main (main) where

import Data.Time.Doomsday (isLeapYear)

main :: IO ()
main = do
  putStrLn "Is 2026 a leap year?"
  print $ isLeapYear 2026
  putStrLn "Is 2024 a leap year?"
  print $ isLeapYear 2024
  putStrLn "Is 2000 a leap year?"
  print $ isLeapYear 2000
