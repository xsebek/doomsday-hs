{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Time.Doomsday
import Data.Time qualified as Time
import System.Console.Haskeline
import System.Random
import System.Random.Stateful
import Options.Applicative hiding (Failure, Success)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Enum (enumerate)
import Data.List (intercalate, dropWhileEnd, partition)
import Data.Char (isSpace)
import Control.Monad (when)

main :: IO ()
main = do
  options <- execParser opts
  case options of
    Explain e -> do
      relative <- (toTime e.date `compare`) <$> today
      -- TODO: detect terminal output by hIsTerminalDevice
      putStrLn . prettyTerm $ evalExplanation e.date doomsdayExplanation { relativeTo = Just relative }
    Train t -> training t.range
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

training :: DateRange -> IO ()
training r = today >>= runInputT defaultSettings . loop . fromTime
 where
  loop :: Date -> InputT IO ()
  loop t = do
    date <- liftIO $ randomDate r t
    let expl = evalExplanation date doomsdayExplanation { relativeTo = Just $ date `compare` t }
    continue <- run date expl
    outputStrLn ""
    if continue then loop t else return () {- TODO: print statistics -}
  run :: Date -> Explanation -> InputT IO Bool
  run date expl = do
    let is = maybe "is" tense expl.relativeTo
    minput <- getInputLine . prettyTerm $ "Which day of the week" <+> is <+> FmtAnn Input (format date) <> "?\n> "
    case partition (=='?') . trim <$> minput of
      Nothing -> return False
      Just (_, "quit") -> return False
      Just (q, "") | not $ null q -> do
        -- TODO: save statistics
        outputPrettyLn $ verboseExpl q expl
        return True
      Just (q, input) -> case parseDayOfWeek input of
        Left e -> do
          let outputErrLn = outputPrettyLn . FmtAnn Failure . format
          when (not $ null input) $ outputErrLn e
          outputErrLn "Type the day of week as digit or name prefix, or type quit/Ctrl+D"
          run date expl
        Right w -> do
          -- TODO: add correct/wrong field inside explanation.
          let correct = Just w == expl.result
          outputPrettyLn $ verboseExpl q expl { correct = Just correct }
          return True

  verboseExpl q expl = case format expl of
    FmtParagraphs ps -> FmtParagraphs . reverse . take (1 + length q) $ reverse ps
    f -> f

outputPrettyLn :: (MonadIO m, Pretty a) => a -> InputT m ()
outputPrettyLn s = haveTerminalUI >>= \t -> outputStrLn ((if t then prettyTerm else pretty) s)

data DateRange = Month | Year | Century | Alltime
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

randomDate :: DateRange -> Date -> IO Date
randomDate dr (Date ty tm td) = do
  c <- randomDateR Alltime (16, 21) (ty `div` 100)
  i <- randomDateR Century (0, 99) (ty `mod` 100)
  let y = c * 100 + i
  m <- randomDateR Year (1, 12) tm
  let maxD = monthLength (isLeapYear y) m
  d <- randomDateR Month (1, maxD) td
  pure $ Date y m d
 where
  randomDateR :: Num a => DateRange -> (Integer, Integer) -> a -> IO a
  randomDateR drMin r v = if dr >= drMin then fromInteger <$> applyAtomicGen (uniformR r) globalStdGen else pure v

today :: IO Time.Day
today = Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime

toTime :: Date -> Time.Day
toTime (Date y m d) = Time.YearMonthDay y (fromEnum m) d

fromTime :: Time.Day -> Date
fromTime (Time.YearMonthDay y m d) = Date y (toEnum m) d

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
