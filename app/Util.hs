module Util where
import Data.Time.Doomsday
import qualified Data.Time as Time

today :: IO Time.Day
today = Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime

toTime :: Date -> Time.Day
toTime (Date y m d) = Time.YearMonthDay y (fromEnum m) d

fromTime :: Time.Day -> Date
fromTime (Time.YearMonthDay y m d) = Date y (toEnum m) d
