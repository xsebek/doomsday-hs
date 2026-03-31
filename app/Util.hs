module Util where
import Data.Time.Doomsday
import Data.List
import Data.Char
import Data.Time qualified as Time

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
