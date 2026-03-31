{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Statistics (
  SaveData (..),
  Entry (..),
  saveData,
  loadData,
  showStatistics,
  plotBoxes,
) where

import Data.Time
import Util
import Data.List.Split (splitOn)
import Data.List (intercalate, partition)
import Data.Time.Format.ISO8601
import System.IO
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Control.Exception (catch, IOException)
import System.Directory (doesFileExist, createDirectoryIfMissing, getXdgDirectory, XdgDirectory (..))
import GHC.Stack (HasCallStack)
import System.FilePath ((</>))
import Granite.String (boxPlot, defPlot, Plot (..))

---------------------------------------------------------------------
-- DATA
---------------------------------------------------------------------

data SaveData = SaveData
  { previous :: [Entry]
  , current :: [Entry]
  }

data Entry = Entry
  { start :: UTCTime
  , date :: Day
  , correct :: DayOfWeek
  , answer :: Maybe DayOfWeek
  , elapsed :: Maybe NominalDiffTime
  }

---------------------------------------------------------------------
-- LOAD/SAVE
---------------------------------------------------------------------

getSaveDir :: IO FilePath
getSaveDir = getXdgDirectory XdgData "doomsday"

saveFileName :: FilePath
saveFileName = "data.csv"

-- | Append entries to the CSV save file of player statistics.
saveData :: HasCallStack => SaveData -> IO ()
saveData d = saveToFile (reverse d.current)

-- | Parse the CSV save file of player statistics.
loadData :: HasCallStack => IO SaveData
loadData = do
  d <- getSaveDir
  let p = d </> saveFileName
  ees <- parseSaveFile p
  es <- case ees of
    Left e -> hPutStrLn stderr ("Failed to read file " <> show p <> ": " <> show e) $> []
    Right s -> pure s
  return $ SaveData (reverse es) []

diffTimeFmt :: String
diffTimeFmt = "%0Es"

weekdayFmt :: String
weekdayFmt = "%a"

saveToFile :: HasCallStack => [Entry] -> IO ()
saveToFile es = do
  d <- getSaveDir
  createDirectoryIfMissing True d
  appendFile (d </> saveFileName) . unlines $ map formatLine es

formatLine :: Entry -> String
formatLine e = intercalate ","
  [ iso8601Show e.start
  , iso8601Show e.date
  , formatTimeDefault weekdayFmt e.correct
  , optionalS (formatTimeDefault weekdayFmt) e.answer
  , optionalS (formatTimeDefault diffTimeFmt) e.elapsed
  ]
 where
  optionalS :: (a -> String) -> Maybe a -> String
  optionalS p = maybe "" p
  formatTimeDefault :: FormatTime t => String -> t -> String
  formatTimeDefault = formatTime defaultTimeLocale

parseSaveFile :: HasCallStack => FilePath -> IO (Either String [Entry])
parseSaveFile p = do
  b <- doesFileExist p
  if b then catch @IOException parse (pure . Left . show) else pure $ Right []
 where
  parse :: HasCallStack => IO (Either String [Entry])
  parse = mapM parseLine . lines <$> readFile p

instance String ~ a => MonadFail (Either a) where 
  fail = Left

type Parser a = String -> Either String a

parseLine :: HasCallStack => Parser Entry
parseLine s = case trim <$> splitOn "," s of
  (t:d:c:a:e:_rest) -> Entry
    <$> iso8601ParseM t
    <*> iso8601ParseM d
    <*> parseTimeDefault weekdayFmt c
    <*> optionalP (parseTimeDefault weekdayFmt) a
    <*> optionalP (parseTimeDefault diffTimeFmt) e
  _ -> Left $ "Invalid format: " <> s
 where
  optionalP :: Parser a -> Parser (Maybe a)
  optionalP p a = if null a then pure Nothing else Just <$> p a
  parseTimeDefault :: ParseTime t => String -> Parser t
  parseTimeDefault = parseTimeM True defaultTimeLocale

---------------------------------------------------------------------
-- STATS
---------------------------------------------------------------------

showStatistics :: SaveData -> String
showStatistics ds = unlines
  [ "Average: " <> if count == 0 then "n/a" else show ((100 * successCount) `div` count) <> "%"
  , "Count correct/wrong/total: " <> show successCount <> "/" <> show failCount <> "/" <> show count
  , "Speed correct/wrong/average: " <> avgSpeed successes <> "/" <> avgSpeed failures <> "/" <> avgSpeed es
  ]
 where
  es = ds.current
  count = length es
  successes = filter (\e -> e.answer == Just e.correct) es
  failures = filter (\e -> not (null e.answer) && e.answer /= Just e.correct) es
  successCount = length successes
  failCount = length failures
  avgSpeed s = if null s then "-" else formatTime defaultTimeLocale "%02Ess" . avgTime $ mapMaybe elapsed s
  avgTime s = sum s / fromIntegral (length s)

allEntries :: SaveData -> [Entry]
allEntries sd = sd.current <> sd.previous

plotBoxes :: Day -> SaveData -> String
plotBoxes (YearMonthDay ty tm _) sd = boxPlot
  [ ("Same month", map secs m)
  , ("Other month", map secs y)
  , ("Other year", map secs c)
  , ("Other centuries", map secs nc)
  ]
  defPlot
    { plotTitle = "Elapsed time to correct answer"
    , yBounds = (Just 0, Nothing)
    }
 where
  (m, nm) = partition ((\(YearMonthDay _ sm _) -> sm == tm) . date) successes
  (y, ny) = partition ((\(YearMonthDay sy _ _) -> sy == ty) . date) nm
  (c, nc) = partition ((\(YearMonthDay sy _ _) -> sy `div` 100 == ty `div` 100) . date) ny

  successes = filter (\e -> e.answer == Just e.correct) $ allEntries sd
  secs e = maybe (error "missing elapsed time") realToFrac e.elapsed
