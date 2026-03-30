{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module REPL (
  trainingREPL,
  DateRange (..),
) where

import Data.Time.Doomsday
import System.Console.Haskeline
import Control.Monad.IO.Class
import Control.Monad (when)
import Data.List
import System.Random.Stateful qualified as R
import Data.Char
import Util
import Data.Functor (($>))

trainingREPL :: DateRange -> IO ()
trainingREPL r = today >>= runInputT defaultSettings . loop . fromTime
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
      Just (q, "") | not $ null q -> showAnswer q expl $> True
      Just (q, input) -> case parseDayOfWeek input of
        Left e -> explainUsage input e >> run date expl
        Right w -> answer q expl w $> True

answer :: [Char] -> Explanation -> DayOfWeek -> InputT IO ()
answer q expl w = do
  outputPrettyLn $ verboseExpl q expl { response = Just w }

showAnswer :: [Char] -> Explanation -> InputT IO ()
showAnswer q expl = do
  -- TODO: save statistics
  outputPrettyLn $ verboseExpl q expl

explainUsage :: String -> String -> InputT IO ()
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

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

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
  randomDateR drMin r v = if dr >= drMin then fromInteger <$> R.applyAtomicGen (R.uniformR r) R.globalStdGen else pure v
