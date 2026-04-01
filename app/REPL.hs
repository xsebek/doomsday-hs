{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL (
  trainingREPL,
) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.Functor (($>))
import Data.List
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time qualified as Time
import Data.Time.Doomsday
import Statistics
import System.Console.Haskeline
import Util
import Data.Char (isSpace)


type App a = InputT (StateT SaveData IO) a


data QueryData = Q
  { date :: Data.Time.Doomsday.Date
  , correct :: Time.DayOfWeek
  , start :: UTCTime
  , explanation :: Explanation
  }


trainingREPL :: DateRange -> IO ()
trainingREPL r = do
  t <- fromTime <$> getToday
  d <- loadData
  flip evalStateT d . runInputT settings $ loop t
 where
  loop :: Date -> App ()
  loop today = do
    time <- liftIO getCurrentTime
    date <- liftIO $ randomDate r today
    let expl = evalExplanation date doomsdayExplanation{relativeTo = Just $ date `compare` today}
    let c = maybe (error "expected evaluated explanation") toTimeD expl.result
    continue <- run $ Q date c time expl
    outputStrLn ""
    if continue
      then loop today
      else do
        sd <- lift get
        liftIO $ saveData sd
        outputStrLn $ showStatistics sd
  run :: QueryData -> App Bool
  run q = do
    let is = maybe "is" tense q.explanation.relativeTo
    let inputDate = FmtAnn Input (format q.date)
    minput <- getInputLine . prettyTerm $ "Which day of the week" <+> is <+> inputDate <> "?\n> "
    case partition (== '?') . trim <$> minput of
      Nothing -> return False
      Just (_, "quit") -> return False
      Just (v, "") | not $ null v -> showAnswer q v $> True
      Just (v, input) -> case parseDayOfWeek input of
        Left e -> explainUsage input e >> run q
        Right w -> evalAnswer q v w $> True


settings :: MonadIO m => Settings m
settings = setComplete (completeWord' Nothing isSpace $ pure . comp) defaultSettings
 where
  comp s = either (const []) (fmap $ simpleCompletion . show) $ matchingDayOfWeek s


type Verbosity = [Char]


evalAnswer :: QueryData -> Verbosity -> DayOfWeek -> App ()
evalAnswer q v w = do
  t <- liftIO getCurrentTime
  let e = Entry q.start (toTime q.date) q.correct (Just $ toTimeD w) (Just $ t `diffUTCTime` q.start)
  lift $ modify (\s -> s{current = e : s.current})
  outputPrettyLn $ verboseExpl v q.explanation{response = Just w}


showAnswer :: QueryData -> Verbosity -> App ()
showAnswer q v = do
  let e = Entry q.start (toTime q.date) q.correct Nothing Nothing
  lift $ modify (\s -> s{current = e : s.current})
  outputPrettyLn $ verboseExpl v q.explanation


explainUsage :: String -> String -> App ()
explainUsage input e = do
  let outputErrLn = outputPrettyLn . FmtAnn Failure . format
  when (not $ null input) $ outputErrLn e
  outputErrLn "Type the day of week as digit or name prefix, ? to see explanation, or type quit/Ctrl+D"


-- | Show an explanation:
-- "" shows just the answer, "?" shows the last part and "???" shows the whole algorithm
verboseExpl :: [Char] -> Explanation -> Format
verboseExpl q expl = case format expl of
  FmtParagraphs ps -> FmtParagraphs . reverse . take (1 + length q) $ reverse ps
  f -> f


outputPrettyLn :: (MonadIO m, Pretty a) => a -> InputT m ()
outputPrettyLn s = haveTerminalUI >>= \t -> outputStrLn ((if t then prettyTerm else pretty) s)
