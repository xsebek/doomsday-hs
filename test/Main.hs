module Main (main) where

import Data.Time.Doomsday
import Test.Tasty
import Test.Tasty.HUnit
import Data.Time.Calendar qualified as Time
import Test.Tasty.QuickCheck
import Data.Function ((&))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Leap year"
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

newtype Y = Y { timeYear :: Time.Year }
  deriving (Show)

instance Arbitrary Y where
  arbitrary :: Gen Y
  arbitrary = Y . (+2000) <$> scale (*2) arbitrary
  shrink :: Y -> [Y]
  shrink = map Y . shrink . timeYear
  