{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad (forM_)
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BS
import Data.Either (isLeft)
import Data.Function ((&))
import Data.List (isInfixOf)
import Data.Time qualified as Time
import Data.Time.Calendar.Month qualified as Time
import Data.Time.Calendar.MonthDay qualified as Time
import Data.Time.Doomsday
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  testGroup
    "Unit tests"
    [ isLeapYearTests
    , monthTests
    , dayOfWeekTests
    , daysFromToTests
    , mnemonicTests
    , expressionTests
    , equationTests
    , explanationTests
    ]


isLeapYearTests :: TestTree
isLeapYearTests =
  testGroup
    "Leap year"
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
monthTests =
  testGroup
    "Month tests"
    [ testCase "Numbering months from 1" $
        fromEnum <$> allMonths @?= timeAllMonths
    , testCase "Order of months in year" $
        forM_ timeAllMonths $ \m ->
          show (toEnum @Month m) @?= timeShow (timeMonth m)
    , testCase "Month length" $
        forM_ [minBound .. maxBound] $ \isLeap ->
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
dayOfWeekTests =
  testGroup
    "Day of Week tests"
    [ testCase "Numbering weekdays from Sunday" $ do
        fromEnum <$> daysOfWeek @?= [0 .. 6]
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
    , testCase "Complete single day" $ matchingDayOfWeek "M" @?= Right [Monday]
    , testCase "Complete multiple days" $ matchingDayOfWeek "T" @?= Right [Tuesday, Thursday]
    , testCase "Complete valid number" $ matchingDayOfWeek "0" @?= Right [Sunday]
    , testCase "Complete invalid number" . assertBool "9" . isLeft $ matchingDayOfWeek "9"
    , testCase "Parse single day number" $ parseDayOfWeek "0" @?= Right Sunday
    , testCase "Parse single day prefix" $ parseDayOfWeek "Su" @?= Right Sunday
    , testCase "Parse single day full" $ parseDayOfWeek "Sunday" @?= Right Sunday
    , testCase "Parse multiple days" . assertBool "S" . isLeft $ parseDayOfWeek "S"
    ]
 where
  timeShow :: Time.DayOfWeek -> String
  timeShow = Time.formatTime Time.defaultTimeLocale "%A"


daysFromToTests :: TestTree
daysFromToTests =
  testGroup
    "Date distance tests"
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


mnemonicTests :: TestTree
mnemonicTests =
  testGroup
    "Mnemonics"
    [ testCase "Default mnemonic has all months" . forM_ [minBound .. maxBound] $ \isLeap -> forM_ allMonths $ \m ->
        let mnms = fst <$> mnMonths isLeap
         in assertBool ("There should be a mnemonic for " <> show m) (m `elem` mnms)
    , testCase "Default mnemonic is correctly on doomsday" . forM_ [2024, 2025] $ \y -> do
        let mkTime (m, d) = Time.YearMonthDay y (fromEnum m) d
        let ds = map mkTime . filter ((/= March) . fst) $ mnMonths (isLeapYear y)
        let expected = if y == 2024 then Time.Thursday else Time.Friday
        forM_ ds $ \d -> assertEqual ("Expected " <> show d <> " to fall on " <> show expected) expected $ Time.dayOfWeek d
    , testGroup
        "Closest doomsday"
        [ testCase "closest to doomsday is doomsday" $ do
            let doom = Date 2026 02 28
            let (mn, d) = closestDoomsday defaultMnemonics doom
            d @?= doom
            assertBool "should mention last Feb" $ "last" `isInfixOf` mn
        , testCase "day before doomsday" $ snd (closestDoomsday defaultMnemonics $ Date 2026 02 27) @?= Date 2026 02 28
        , testCase "day after doomsday" $ snd (closestDoomsday defaultMnemonics $ Date 2026 03 01) @?= Date 2026 03 00
        ]
    ]
 where
  mnMonths isLeap = concatMap dates (defaultMnemonics `forLeapYear` isLeap)


expressionTests :: TestTree
expressionTests =
  testGroup
    "Expressions"
    [ expression "1" 1 1
    , expression "Tuesday" (EDay Tuesday) 2
    , expressionWithVars [('X', 5), ('Y', 7)] "Y" (EVar 'Y') 7
    , expression "-10" (-10) (-10)
    , expression "-(-10)" (-(-10)) 10
    , expression "1 + 2" (1 + 2) 3
    , expression "1 - 2" (1 - 2) (-1)
    , expression "3 * 3" (3 * 3) 9
    , expression "12 / 4" (12 `div` 4) 3
    , expression "1 + 2*3" (1 + (2 * 3)) 7
    , expression "(1 + 2) * 3" ((1 + 2) * 3) 9
    , expression "8 % 7" (8 `mod` 7) 1
    , expression "(4 + 5) % 7" ((4 + 5) `mod` 7) 2
    , expressionWithVars [('I', 5)] "Tuesday + I" (EDay Tuesday + EVar 'I') 7
    -- TODO: (2 + 2) + 2/4
    ]
 where
  expressionWithVars v pret e res = testCase pret $ do
    eval v e @?= res
    pretty e @?= pret
  expression = expressionWithVars []


equationTests :: TestTree
equationTests =
  testGroup
    "Equations"
    [ testCase "uniq res" $ uniq (EqRes 1) @?= EqRes 1
    , testCase "uniq same var" $ uniq ('x' ^== 'x') @?= toEquation 'x'
    , testCase "uniq diff var" $ uniq ('x' ^== 'y') @?= ('x' ^== 'y')
    , testCase "lift equiv" $ ('x' ^== (2 :: Int) ^=== Tuesday) @?= (EVar 'x' :== EConst 2 :=== EqRes (EDay Tuesday))
    ]


explanationTests :: TestTree
explanationTests =
  testGroup
    "Explanations"
    [ testCase "Part builder returns result variable" $ do
        centuryVar @?= EVar 'A'
        yearVar @?= EVar 'W'
    , testGroup "Century" $
        [ goldenVsString "Pretty abstract century explanation" "test/data/century_abstract.golden" $
            prettyIO centuryExpl
        , goldenVsString "Pretty evaluated century explanation" "test/data/century_evaluated.golden" $
            prettyIO (evalExplanation d centuryExpl)
        ]
    , testGroup "Year" $
        [ goldenVsString "Pretty abstract year explanation" "test/data/year_abstract.golden" $
            prettyIO yearExpl
        , goldenVsString "Pretty evaluated year explanation" "test/data/year_evaluated.golden" $
            prettyIO (evalExplanationWith [('A', EDay Tuesday)] yearExpl)
        ]
    , testGroup "Year with division by 4" $
        [ goldenVsString "Pretty abstract year explanation" "test/data/year4_abstract.golden" $
            prettyIO yearExpl4
        , goldenVsString "Pretty evaluated year explanation" "test/data/year4_evaluated.golden" $
            prettyIO (evalExplanationWith [('A', EDay Tuesday)] yearExpl4)
        ]
    , testGroup "Year with odd + 11" $
        [ goldenVsString "Pretty abstract year explanation" "test/data/year11_abstract.golden" $
            prettyIO yearExp11
        , goldenVsString "Pretty evaluated year explanation" "test/data/year11_evaluated.golden" $
            prettyIO (evalExplanationWith [('A', EDay Tuesday)] yearExp11)
        ]
    , testGroup "Weekday" $
        [ goldenVsString "Pretty abstract weekday explanation" "test/data/week_abstract.golden" $
            prettyIO weekExpl
        , goldenVsString "Pretty evaluated weekday explanation" "test/data/week_evaluated.golden" $
            prettyIO (evalExplanationWith [('W', EDay Saturday)] weekExpl)
        ]
    , testGroup "Whole explanation" $
        [ goldenVsString "Pretty abstract whole explanation" "test/data/whole_abstract.golden" $
            prettyIO doomsdayExplanation
        , goldenVsString "Pretty evaluated whole explanation" "test/data/whole_evaluated.golden" $
            prettyIO (evalExplanation d doomsdayExplanation){relativeTo = Just EQ}
        , goldenVsString "Terminal abstract whole explanation" "test/data/terminal_abstract.golden" $
            prettyTermIO doomsdayExplanation
        , goldenVsString "Terminal evaluated whole explanation" "test/data/terminal_evaluated.golden" $
            prettyTermIO (evalExplanation d doomsdayExplanation){relativeTo = Just EQ}
        , goldenVsString "Terminal evaluated whole correct explanation" "test/data/terminal_correct.golden" $
            prettyTermIO (evalExplanation d doomsdayExplanation){relativeTo = Just LT, response = Just Friday}
        , goldenVsString "Terminal evaluated whole correct explanation" "test/data/terminal_wrong.golden" $
            prettyTermIO (evalExplanation d doomsdayExplanation){relativeTo = Just GT, response = Just Monday}
        ]
    , testGroup "Check explanation result against Time" $
        [ sameDayAsTime "Conways" doomsdayExplanation
        , sameDayAsTime "Div 4" doomsdayExplanationDiv4
        , sameDayAsTime "Odd 11" doomsdayExplanationOdd11
        ]
    ]
 where
  d = Date 2026 02 27
  emptyExpl = Explanation [] Nothing Nothing Nothing
  (centuryExpl, centuryVar) = runState findCenturyAnchor emptyExpl
  (yearExpl, yearVar) = runState (findYearAnchor centuryVar) emptyExpl
  (yearExpl4, _) = runState (findYearAnchorDiv4 centuryVar) emptyExpl
  (yearExp11, _) = runState (findYearAnchorOdd11 centuryVar) emptyExpl
  (weekExpl, _) = runState (findWeekday yearVar) emptyExpl
  evalExplanationWith vars = snd . flip runState vars . evalExplanationS d
  prettyIO :: (Pretty a) => a -> IO BS.ByteString
  prettyIO = pure . toLazyByteString . mconcat . map charUtf8 . pretty
  prettyTermIO :: (Pretty a) => a -> IO BS.ByteString
  prettyTermIO = pure . toLazyByteString . mconcat . map charUtf8 . prettyTerm
  sameDayAsTime :: String -> Explanation -> TestTree
  sameDayAsTime name expl = testProperty name $ \(D td) ->
    let dd = toDate td
        eExpl = evalExplanation dd expl
        expected = toEnum @DayOfWeek . fromEnum $ Time.dayOfWeek td
     in case eExpl.result of
          Nothing -> property False
          Just res -> (res === expected) & tabulate "century" [(<> "th") . show $ dd.year `div` 100]


-- -----------------------------------------------------------------
-- Arbitrary instances
-- -----------------------------------------------------------------

newtype Y = Y {timeYear :: Time.Year}
  deriving (Show)


instance Arbitrary Y where
  arbitrary :: Gen Y
  arbitrary = Y . (+ 2000) <$> scale (* 2) arbitrary
  shrink :: Y -> [Y]
  shrink = map Y . shrink . subtract 2000 . timeYear


newtype D = D {timeDay :: Time.Day}
  deriving (Show)


instance Arbitrary D where
  arbitrary :: Gen D
  arbitrary = fmap D $ Time.YearMonthDay <$> scale (`div` 10) (fmap timeYear arbitrary) <*> choose (1, 12) <*> choose (1, 31)
  shrink :: D -> [D]
  shrink = shrinkMap (D . flip Time.addDays d2000) (flip Time.diffDays d2000 . timeDay)


d2000 :: Time.Day
d2000 = Time.YearMonthDay 2000 01 01


toDate :: Time.Day -> Date
toDate (Time.YearMonthDay y m d) = Date y (toEnum m) d
