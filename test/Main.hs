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
  ]
 where
  timeAllMonths :: [Time.MonthOfYear]
  timeAllMonths = [Time.January .. Time.December]
  timeMonth :: Time.MonthOfYear -> Time.Month
  timeMonth = Time.YearMonth 0
  timeShow :: Time.Month -> String
  timeShow m = Time.formatTime Time.defaultTimeLocale "%B" m 

-- -----------------------------------------------------------------
-- Arbitrary instances
-- -----------------------------------------------------------------

newtype Y = Y { timeYear :: Time.Year }
  deriving (Show)

instance Arbitrary Y where
  arbitrary :: Gen Y
  arbitrary = Y . (+2000) <$> scale (*2) arbitrary
  shrink :: Y -> [Y]
  shrink = map Y . shrink . timeYear
