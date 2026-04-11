{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (intercalate)
import Data.Time.Doomsday
import Options.Applicative
import REPL
import Statistics (loadData, plotBoxes, plotLine, showStatistics)
import Util


main :: IO ()
main = do
  options <- execParser opts
  case options of
    Explain e -> do
      relative <- (toTime e.date `compare`) <$> getToday
      -- TODO: detect terminal output by hIsTerminalDevice
      putStrLn . prettyTerm $ evalExplanation e.date doomsdayExplanation{relativeTo = Just relative}
    Train t -> trainingREPL t.range
    Stats p ->
      loadData >>= case p of
        Bars -> putStrLn . plotBoxes
        Line -> putStrLn . plotLine
        List -> putStrLn . showStatistics
 where
  opts =
    info
      (parser <**> helper)
      ( header "doomsday - mentally calculate weekday"
          <> progDesc "Doomsday algorithm training program to calculate the day of week for any date."
          <> fullDesc
      )


parser :: Parser Command
parser =
  hsubparser
    ( command "explain" (info explainCommand (progDesc "Explain the doomsday algorithm for a given date"))
        <> command "train" (info trainCommand (progDesc "Train the doomsday algorithm in a read-eval loop"))
        <> command "plot" (info plotCommand (progDesc "Show plots and statistics."))
    )


explainCommand :: Parser Command
explainCommand =
  Explain . ExplainParams
    <$> argument (eitherReader readIsoDate) (metavar "YYYY-MM-DD")


trainCommand :: Parser Command
trainCommand =
  Train . TrainParams
    <$> option
      auto
      ( long "range"
          <> short 'r'
          <> metavar "RANGE"
          <> value Year
          <> help ("Pick random dates from given time range " <> intercalate "|" (show @DateRange <$> [minBound .. maxBound]))
          <> showDefault
      )


plotCommand :: Parser Command
plotCommand =
  Stats
    <$> hsubparser
      ( command "bars" (info (pure Bars) idm)
          <> command "line" (info (pure Line) idm)
          <> command "list" (info (pure List) idm)
      )


data Command
  = Explain ExplainParams
  | Train TrainParams
  | Stats StatsParams


data ExplainParams = ExplainParams {date :: Date}


data TrainParams = TrainParams {range :: DateRange}


data StatsParams = Bars | Line | List
