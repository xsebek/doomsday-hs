{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Time.Doomsday
import Options.Applicative
import Data.Enum (enumerate)
import Data.List (intercalate)
import REPL
import Util

main :: IO ()
main = do
  options <- execParser opts
  case options of
    Explain e -> do
      relative <- (toTime e.date `compare`) <$> today
      -- TODO: detect terminal output by hIsTerminalDevice
      putStrLn . prettyTerm $ evalExplanation e.date doomsdayExplanation { relativeTo = Just relative }
    Train t -> trainingREPL t.range
 where
  opts = info (parser <**> helper)
      ( header "doomsday - mentally calculate weekday"
      <> progDesc "Doomsday algorithm training program to calculate the day of week for any date."
      <> fullDesc
      )

parser :: Parser Command
parser = hsubparser
  ( command "explain" (info explainCommand ( progDesc "Explain the doomsday algorithm for a given date" ))
  <> command "train" (info trainCommand ( progDesc "Train the doomsday algorithm in a read-eval loop" ))
  )

explainCommand :: Parser Command
explainCommand = Explain . ExplainParams
  <$> argument (eitherReader readIsoDate) (metavar "YYYY-MM-DD")

trainCommand :: Parser Command
trainCommand = Train . TrainParams
  <$> option auto
    ( long "range" <> short 'r' <> metavar "RANGE" <> value Year
     <> help ("Pick random dates from given time range " <> intercalate "|" (show <$> enumerate @DateRange))  <> showDefault)

data Command 
  = Explain ExplainParams
  | Train TrainParams

data ExplainParams = ExplainParams { date :: Date }

data TrainParams = TrainParams { range :: DateRange }
