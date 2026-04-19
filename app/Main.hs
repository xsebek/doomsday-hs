{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Data.List (intercalate)
import Data.Time.Doomsday
import Options.Applicative
import REPL
import Statistics
import Util


main :: IO ()
main = do
  options <- execParser opts
  case options of
    Explain e -> do
      relative <- (toTime e.date `compare`) <$> getToday
      -- TODO: detect terminal output by hIsTerminalDevice
      putStrLn . prettyTerm $ evalExplanation e.date (explanationForm e.formula){relativeTo = Just relative}
    Train t -> trainingREPL t.formula t.range
    Stats -> loadData >>= putStrLn . showRangeStatistics
    Plot Boxes -> loadData >>= putStrLn . plotBoxes
    Plot Line -> loadData >>= putStrLn . plotLine
 where
  opts =
    info
      (parser <**> helper)
      ( header "doomsday - mentally calculate weekday"
          <> progDesc "Doomsday algorithm training program to calculate the day of week for any date."
          <> fullDesc
      )

data Command
  = Explain ExplainParams
  | Train TrainParams
  | Plot PlotParams
  | Stats

parser :: Parser Command
parser =
  hsubparser
    ( command "explain" (info explainCommand (progDesc "Explain the doomsday algorithm for a given date"))
        <> command "train" (info trainCommand (progDesc "Train the doomsday algorithm in a read-eval loop"))
        <> command "plot" (info plotCommand (progDesc "Show plots of accuracy and speed."))
        <> command "stats" (info (pure Stats) (progDesc "List statistics of accuracy and speed."))
    )

data ExplainParams = ExplainParams
  { formula :: Formula
  , date :: Date
  }

explainCommand :: Parser Command
explainCommand =
  fmap Explain $ ExplainParams
    <$> parseFormula
    <*> argument (eitherReader readIsoDate) (metavar "YYYY-MM-DD")

data TrainParams = TrainParams
  { formula :: Formula
  , range :: DateRange
  }

trainCommand :: Parser Command
trainCommand =
  fmap Train $ TrainParams
    <$> parseFormula
    <*> option
      auto
      ( long "range"
          <> short 'r'
          <> metavar "RANGE"
          <> value Year
          <> help ("Pick random dates from given time range " <> intercalate "|" (show @DateRange <$> [minBound .. maxBound]))
          <> showDefault
      )

data PlotParams = Boxes | Line

plotCommand :: Parser Command
plotCommand =
  Plot
    <$> hsubparser
      ( command "boxes" (info (pure Boxes) idm)
          <> command "line" (info (pure Line) idm)
      )

parseFormula :: Parser Formula
parseFormula = option
      auto
      ( long "formula"
          <> short 'f'
          <> metavar "FORMULA"
          <> value Conways
          <> help ("Use Doomsday formula " <> intercalate "|" (show @Formula <$> [minBound .. maxBound]))
          <> showDefault
      )
