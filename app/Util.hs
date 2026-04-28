module Util where

import Data.Char
import Data.List
import Data.Time qualified as Time
import Data.Time.Doomsday
import System.Random.Stateful qualified as R


getToday :: IO Time.Day
getToday = Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime


toTime :: Date -> Time.Day
toTime (Date y m d) = Time.YearMonthDay y (fromEnum m) d


fromTime :: Time.Day -> Date
fromTime (Time.YearMonthDay y m d) = Date y (toEnum m) d


toTimeD :: DayOfWeek -> Time.DayOfWeek
toTimeD = toEnum . fromEnum


trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace


data DateRange = Month | Year | Century | Alltime
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


randomDate :: DateRange -> Date -> IO Date
randomDate dr (Date ty tm td) = do
  c <- randomDateR Alltime (16, 21) (ty `div` 100)
  i <- randomDateR Century (0, 99) (ty `mod` 100)
  let y = c * 100 + i
  m <- randomDateR Year (1, 12) tm
  let maxD = monthLength (isLeapYear y) m
  d <- randomDateR Month (1, maxD) td
  pure $ Date y m d
 where
  randomDateR :: (Num a) => DateRange -> (Integer, Integer) -> a -> IO a
  randomDateR drMin r v = if dr >= drMin then fromInteger <$> R.applyAtomicGen (R.uniformR r) R.globalStdGen else pure v


data Formula = Conways | Div4 | Odd11 | Nakai
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


explanationForm :: Formula -> Explanation
explanationForm = \case
  Conways -> doomsdayExplanation
  Div4 -> doomsdayExplanationDiv4
  Odd11 -> doomsdayExplanationOdd11
  Nakai -> doomsdayExplanationNakai
