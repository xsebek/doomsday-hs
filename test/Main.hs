{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultilineStrings #-}

module Main (main) where

import Data.Time.Doomsday
import Test.Tasty
import Test.Tasty.HUnit
import Data.Time qualified as Time
import Data.Time.Calendar.Month qualified as Time
import Test.Tasty.QuickCheck
import Data.Function ((&))
import Control.Monad (forM_)
import qualified Data.Time.Calendar.MonthDay as Time
import Data.Enum (enumerate)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [ isLeapYearTests
  , monthTests
  , dayOfWeekTests
  , daysFromToTests
  , expressionTests
  , explanationTests
  ]

isLeapYearTests :: TestTree
isLeapYearTests = testGroup "Leap year"
  [ testCase "odd" $ isLeapYear 2025 @?= False
  , testCase "even" $ isLeapYear 2026 @?= False
  , testCase "leap" $ isLeapYear 2020 @?= True
  , testCase "odd century" $ isLeapYear 2100 @?= False
  , testCase "4th century" $ isLeapYear 2000 @?= True
  , testProperty "same as time" $ \(Y y) ->
    isLeapYear y === Time.isLeapYear y
     & classify (Time.isLeapYear y) "leap years"
     & tabulate "century" [(<> "th") . show $ y `div` 100]
  ]

monthTests :: TestTree
monthTests = testGroup "Month tests"
  [ testCase "Numbering months from 1" $
    fromEnum <$> allMonths @?= timeAllMonths
  , testCase "Order of months in year" $
    forM_ timeAllMonths $ \m ->
      show (toEnum @Month m) @?= timeShow (timeMonth m)
  , testCase "Month length" $ 
    forM_ enumerate $ \isLeap ->
      forM_ timeAllMonths $ \m ->
        monthLength isLeap (toEnum m) @?= Time.monthLength isLeap m
  , testCase "Month number" $ toEnum 13 @?= January
  , testCase "Month numbers" $ do
    1 @?= January
    12 @?= December
    0 @?= December
    -1 @?= November
    13 @?= January
  ]
 where
  timeAllMonths :: [Time.MonthOfYear]
  timeAllMonths = [Time.January .. Time.December]
  timeMonth :: Time.MonthOfYear -> Time.Month
  timeMonth = Time.YearMonth 0
  timeShow :: Time.Month -> String
  timeShow = Time.formatTime Time.defaultTimeLocale "%B"

dayOfWeekTests :: TestTree
dayOfWeekTests = testGroup "Day of Week tests"
  [ testCase "Numbering weekdays from Sunday" $ do
    fromEnum <$> daysOfWeek @?= [0..6]
    show <$> daysOfWeek @?= timeShow <$> [Time.Sunday .. Time.Saturday]
  , testCase "Counting with weekdays" $ do
    Sunday + Sunday @?= Sunday
    Sunday + Monday @?= Monday
    Monday + Wednesday @?= Thursday
    Friday - Saturday @?= Saturday
    negate Friday @?= Tuesday
  , testCase "Using numbers as weekdays" $ do
    1 @?= Monday
    Tuesday * 5 @?= Wednesday
    Thursday `div` 4 @?= Monday
  ]
 where
  timeShow :: Time.DayOfWeek -> String
  timeShow = Time.formatTime Time.defaultTimeLocale "%A"

daysFromToTests :: TestTree
daysFromToTests = testGroup "Date distance tests"
  [ testCase "Same date is 0 days" $ daysFromTo (Date 2000 01 01) (Date 2000 01 01) @?= 0
  , testCase "Next date is 1 days" $ daysFromTo (Date 2000 01 01) (Date 2000 01 02) @?= 1
  , testCase "Yesterday is -1 days" $ daysFromTo (Date 2000 01 02) (Date 2000 01 01) @?= -1
  , testCase "Next month is 31 days" $ daysFromTo (Date 2000 01 01) (Date 2000 02 01) @?= 31
  , testCase "Next month end is 59 days" $ daysFromTo (Date 2000 01 01) (Date 2000 02 29) @?= 59
  , testCase "Next month middle is 40 days" $ daysFromTo (Date 2000 01 11) (Date 2000 02 20) @?= 40
  , testProperty "Date diff same as time" $ \(D a) (D b) ->
    let (da, db) = (toDate a, toDate b)
    in daysFromTo (toDate a) (toDate b) === fromInteger (b `Time.diffDays` a)
      & classify (da.year == db.year) "same year"
  ]
 where
  toDate (Time.YearMonthDay y m d) = Date y (toEnum m) d

expressionTests :: TestTree
expressionTests = testGroup "Expressions"
  [ expression "1" 1 1
  , expression "Tuesday" (EDay Tuesday) 2
  , expressionWithVars [('X', 5), ('Y', 7)] "Y" (EVar 'Y') 7
  , expression "-10" (-10) (-10) 
  , expression "-(-10)" (-(-10)) 10
  , expression "1 + 2" (1 + 2) 3
  , expression "1 - 2" (1 - 2) (-1)
  , expression "3 * 3" (3 * 3) 9
  , expression "12 / 4" (12 `div` 4) 3
  , expression "1 + 2 * 3" (1 + (2 * 3)) 7
  , expression "(1 + 2) * 3" ((1 + 2) * 3) 9 
  , expression "8 % 7" (8 `mod` 7) 1
  , expression "(4 + 5) % 7" ((4 + 5) `mod` 7) 2
  , expressionWithVars [('I', 5)] "Tuesday + I" (EDay Tuesday + EVar 'I') 7
  ]
 where
  expressionWithVars v pret e res = testCase pret $ do
    eval v e @?= res
    pretty e @?= pret
  expression = expressionWithVars []

explanationTests :: TestTree
explanationTests = testGroup "Explanations"
  [ testCase "Pretty abstract explanation" $ do
    let (s, e) = runState findCenturyAnchor $ Explanation []
    e @?= EVar 'A'
    pretty s @?= """
      Find the century anchor. Starting with year Y:
       - take the century digits C = Y / 100
       - in a four century cycle its index is F = C % 4
       - the resulting increment is I = F * 5
       - add Tuesday and get A = Tuesday + I\n
      """
  ]

-- -----------------------------------------------------------------
-- Arbitrary instances
-- -----------------------------------------------------------------

newtype Y = Y { timeYear :: Time.Year }
  deriving (Show)

instance Arbitrary Y where
  arbitrary :: Gen Y
  arbitrary = Y . (+2000) <$> scale (*2) arbitrary
  shrink :: Y -> [Y]
  shrink = map Y . shrink . subtract 2000 . timeYear

newtype D = D { timeDay :: Time.Day }
  deriving (Show)

instance Arbitrary D where
  arbitrary :: Gen D
  arbitrary = fmap D $ Time.YearMonthDay <$> scale (`div` 10) (fmap timeYear arbitrary) <*> choose (1, 12) <*> choose (1, 31)
  shrink :: D -> [D]
  shrink = shrinkMap (D . flip Time.addDays d2000) (flip Time.diffDays d2000 . timeDay)

d2000 :: Time.Day
d2000 = Time.YearMonthDay 2000 01 01
