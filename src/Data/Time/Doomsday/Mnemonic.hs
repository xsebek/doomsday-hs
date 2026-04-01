{-# LANGUAGE OverloadedRecordDot #-}

module Data.Time.Doomsday.Mnemonic (
  DoomsdayMnemonics (..),
  Mnemonic (..),
  defaultMnemonics,
  closestDoomsday,
) where

import Data.Foldable1 qualified as F1
import Data.List.NonEmpty (NonEmpty (..), sortWith)
import Data.Time.Doomsday.Date
import Data.Time.Doomsday.Month
import Data.Time.Doomsday.Year (isLeapYear)


newtype DoomsdayMnemonics = DoomsdayMnemonics {forLeapYear :: Bool -> [Mnemonic]}


data Mnemonic = Mnemonic
  { description :: String
  , dates :: [(Month, Int)]
  }


defaultMnemonics :: DoomsdayMnemonics
defaultMnemonics = DoomsdayMnemonics $ \isLeap ->
  [ Mnemonic "the 3rd 3 years in 4 and the 4th in the 4th" [(January, if isLeap then 4 else 3)]
  , Mnemonic "the last day of February" [(February, if isLeap then 29 else 28), (March, 0)]
  , Mnemonic "nth in even months" [(April, 4), (June, 6), (August, 8), (October, 10), (December, 12)]
  , Mnemonic "9-to-5" [(May, 9), (September, 5)]
  , Mnemonic "7-11" [(July, 11), (November, 7)]
  ]


closestDoomsday :: DoomsdayMnemonics -> Date -> (String, Date)
closestDoomsday dms (Date y m d) =
  Date y m <$> case snd <$> filter ((m ==) . fst) ds of
    [] -> error $ "No doomsday mnemonic for month " <> show m
    [dd] -> dd
    (dd : dds) -> F1.head $ sortWith (abs . subtract d . snd) (dd :| dds)
 where
  ds = concatMap expand ms
  ms = dms `forLeapYear` isLeapYear y


expand :: Mnemonic -> [(Month, (String, Int))]
expand mn = map (\(m, d) -> (m, (mn.description, d))) mn.dates
