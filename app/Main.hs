module Main (main) where

import Data.Time.Doomsday
import System.Environment (getArgs)
import Control.Monad
import System.IO (hPutStrLn, stderr)
import Data.Time qualified as Time

main :: IO ()
main = do
  args <- getArgs
  today <- Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime
  case args of
    [] -> hPutStrLn stderr $ "Empty aruments, expected date YYYY-MM-DD"
    _ -> forM_ args $ \a -> case readYMD a of
      Nothing -> hPutStrLn stderr $ "Cannot parse date '" <> a <> "', expected YYYY-MM-DD"
      Just d -> putStrLn . pretty $ evalExplanation d doomsdayExplanation {relativeTo = Just $ toTime d `compare` today }

toTime :: Date -> Time.Day
toTime (Date y m d) = Time.YearMonthDay y (fromEnum m) d
