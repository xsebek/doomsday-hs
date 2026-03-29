{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where

import Data.Time.Doomsday
import Data.Time qualified as Time
import System.Console.Haskeline
import System.Random
import System.Random.Stateful
import Options.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import Data.Enum (enumerate)
import Data.List (intercalate)

main :: IO ()
main = do
  options <- execParser opts
  case options of
    Explain e -> do
      relative <- (toTime e.date `compare`) <$> today
      putStrLn . pretty $ evalExplanation e.date doomsdayExplanation { relativeTo = Just relative }
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
  <$> argument isoDate (metavar "YYYY-MM-DD")

isoDate :: ReadM Date
isoDate = eitherReader $ \case
  [y1,y2,y3,y4,'-',m1,m2,'-',d1,d2]
    | all isDigit [y1,y2,y3,y4,m1,m2,d1,d2]
    -> Right $ Date (read [y1,y2,y3,y4]) (toEnum $ read [m1,m2]) (read [d1,d2])
  _ -> Left "Expected date in format YYYY-MM-DD"

trainCommand :: Parser Command
trainCommand = Train . TrainParams
  <$> option auto (long "range" <> short 'r' <> metavar "RANGE" <> value Year
         <> help ("Pick random dates from given time range " <> intercalate "|" (show <$> enumerate @DateRange))  <> showDefault)

data Command 
  = Explain ExplainParams
  | Train TrainParams

data ExplainParams = ExplainParams { date :: Date }

data TrainParams = TrainParams { range :: DateRange }

today :: IO Time.Day
today = Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime

toTime :: Date -> Time.Day
toTime (Date y m d) = Time.YearMonthDay y (fromEnum m) d

fromTime :: Time.Day -> Date
fromTime (Time.YearMonthDay y m d) = Date y (toEnum m) d

training :: DateRange -> IO ()
training r = today >>= runInputT defaultSettings . loop . fromTime
   where
       loop :: Date -> InputT IO ()
       loop t = do
           date <- liftIO $ randomDate r t
           minput <- getInputLine $ "Which day of the week was " <> pretty date <> "?\n> "
           outputStrLn . pretty $ evalExplanation date doomsdayExplanation { relativeTo = Just $ date `compare` t }
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ "Input was: " ++ input
                                loop t

data DateRange = Month | Year | Century | Alltime
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

randomDate :: DateRange -> Date -> IO Date
randomDate dr (Date ty tm td) = do
  c <- randomDateR Alltime (16, 21) (ty `div` 100)
  i <- randomDateR Century (0, 99) (ty `mod` 100)
  let y = c * 100 + i
  m <- randomDateR Century (0, 12) tm
  let maxD = monthLength (isLeapYear y) m
  d <- randomDateR Month (0, maxD) td
  pure $ Date y m d
 where
  randomDateR :: Num a => DateRange -> (Integer, Integer) -> a -> IO a
  randomDateR drMin r v = if dr >= drMin then fromInteger <$> applyAtomicGen (uniformR r) globalStdGen else pure v
