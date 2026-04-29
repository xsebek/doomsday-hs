{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (intercalate)
import Data.Proxy (Proxy (..))
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
    Repl t -> trainingREPL t.formula t.range
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
  | Repl ReplParams
  | Plot PlotParams
  | Stats


parser :: Parser Command
parser =
  hsubparser . mconcat $
    [ command "explain" (info (Explain <$> explainParams) (progDesc "Explain the doomsday algorithm for a given date"))
    , command "repl" (info (Repl <$> trainParams) (progDesc "Train the doomsday algorithm in a read-eval-print loop"))
    , command "plot" (info (Plot <$> plotParams) (progDesc "Show plots of accuracy and speed."))
    , command "stats" (info (pure Stats) (progDesc "List statistics of accuracy and speed."))
    ]


data ExplainParams = ExplainParams
  { formula :: Formula
  , date :: Date
  }


explainParams :: Parser ExplainParams
explainParams =
  ExplainParams
    <$> formulaOption
    <*> parseDate
 where
  parseDate = argument (eitherReader readIsoDate) (metavar "YYYY-MM-DD")


data ReplParams = TrainParams
  { formula :: Formula
  , range :: DateRange
  }


trainParams :: Parser ReplParams
trainParams =
  TrainParams
    <$> formulaOption
    <*> parseRange
 where
  parseRange =
    option auto . mconcat $
      [ long "range"
      , short 'r'
      , metavar "RANGE"
      , value Year
      , help ("Pick random dates from given time range " <> showValues (Proxy @DateRange))
      , showDefault
      ]


data PlotParams = Boxes | Line


plotParams :: Parser PlotParams
plotParams =
  hsubparser . mconcat $
    [ command "boxes" (info (pure Boxes) idm)
    , command "line" (info (pure Line) idm)
    ]


formulaOption :: Parser Formula
formulaOption =
  option auto . mconcat $
    [ long "formula"
    , short 'f'
    , metavar "FORMULA"
    , value Conways
    , help ("Use Doomsday formula " <> showValues (Proxy @Formula))
    , showDefault
    ]


showValues :: forall a. (Bounded a, Enum a, Show a) => Proxy a -> String
showValues _ = "[" <> intercalate "|" (show @a <$> [minBound .. maxBound]) <> "]"
