{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Statistics (
  SaveData (..),
  Entry (..),
  saveData,
  loadData,
  showRangeStatistics,
  showStatistics,
  plotBoxes,
  plotLine,
) where

import Control.Exception (IOException, catch)
import Data.Functor (($>))
import Data.List (intercalate, partition)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Time
import Data.Time.Format.ISO8601
import Granite.String (Plot (..), boxPlot, defPlot, lineGraph)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath ((</>))
import System.IO
import Util


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


isCorrect :: Entry -> Bool
isCorrect e = e.answer == Just e.correct


isWrong :: Entry -> Bool
isWrong e = not (null e.answer) && e.answer /= Just e.correct


---------------------------------------------------------------------
-- LOAD/SAVE
---------------------------------------------------------------------

getSaveDir :: IO FilePath
getSaveDir = getXdgDirectory XdgData "doomsday"


saveFileName :: FilePath
saveFileName = "data.csv"


-- | Append entries to the CSV save file of player statistics.
saveData :: SaveData -> IO ()
saveData d = saveToFile (reverse d.current)


-- | Parse the CSV save file of player statistics.
loadData :: IO SaveData
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


saveToFile :: [Entry] -> IO ()
saveToFile es = do
  d <- getSaveDir
  createDirectoryIfMissing True d
  appendFile (d </> saveFileName) . unlines $ map formatLine es


formatLine :: Entry -> String
formatLine e =
  intercalate
    ","
    [ iso8601Show e.start
    , iso8601Show e.date
    , formatTimeDefault weekdayFmt e.correct
    , optionalS (formatTimeDefault weekdayFmt) e.answer
    , optionalS (formatTimeDefault diffTimeFmt) e.elapsed
    ]
 where
  optionalS :: (a -> String) -> Maybe a -> String
  optionalS p = maybe "" p
  formatTimeDefault :: (FormatTime t) => String -> t -> String
  formatTimeDefault = formatTime defaultTimeLocale


parseSaveFile :: FilePath -> IO (Either String [Entry])
parseSaveFile p = do
  b <- doesFileExist p
  if b then catch @IOException parse (pure . Left . show) else pure $ Right []
 where
  parse :: IO (Either String [Entry])
  parse = mapM parseLine . lines <$> readFile p


instance (String ~ a) => MonadFail (Either a) where
  fail = Left


type Parser a = String -> Either String a


parseLine :: Parser Entry
parseLine s = case trim <$> splitOn "," s of
  (t : d : c : a : e : _rest) ->
    Entry
      <$> iso8601ParseM t
      <*> iso8601ParseM d
      <*> parseTimeDefault weekdayFmt c
      <*> optionalP (parseTimeDefault weekdayFmt) a
      <*> optionalP (parseTimeDefault diffTimeFmt) e
  _ -> Left $ "Invalid format: " <> s
 where
  optionalP :: Parser a -> Parser (Maybe a)
  optionalP p a = if null a then pure Nothing else Just <$> p a
  parseTimeDefault :: (ParseTime t) => String -> Parser t
  parseTimeDefault = parseTimeM True defaultTimeLocale


---------------------------------------------------------------------
-- STATS
---------------------------------------------------------------------

showRangeStatistics :: SaveData -> String
showRangeStatistics sd =
  unlines
    [ "Overall"
    , showStatistics $ allEntries sd
    , "Same month"
    , get Month
    , "Other month"
    , get Year
    , "Other year"
    , get Century
    , "Other centuries"
    , get Alltime
    ]
 where
  rs = partitionEntries $ allEntries sd
  get r = showStatistics . concatMap snd $ filter ((== r) . fst) rs


showStatistics :: [Entry] -> String
showStatistics es =
  unlines
    [ " * Average: " <> if count == 0 then "n/a" else show ((100 * successCount) `div` count) <> "%"
    , " * Count correct/wrong/total: " <> show successCount <> "/" <> show failCount <> "/" <> show count
    , " * Speed correct/wrong/average: " <> avgSpeed successes <> "/" <> avgSpeed failures <> "/" <> avgSpeed es
    ]
 where
  count = length es
  successes = filter isCorrect es
  failures = filter isWrong es
  successCount = length successes
  failCount = length failures
  avgSpeed ms = let s = mapMaybe elapsed ms in if null s then "-" else formatTime defaultTimeLocale "%02Ess" $ avgTime s
  avgTime s = sum s / fromIntegral (length s)


allEntries :: SaveData -> [Entry]
allEntries sd = sd.current <> sd.previous


plotBoxes :: SaveData -> String
plotBoxes sd =
  boxPlot
    [ ("Same month", get Month)
    , ("Other month", get Year)
    , ("Other year", get Century)
    , ("Other centuries", get Alltime)
    ]
    defPlot
      { plotTitle = "Elapsed time to correct answer"
      , yBounds = (Just 0, Nothing)
      }
 where
  successes = filter isCorrect $ allEntries sd
  rs = partitionEntries successes
  secs e = maybe (error "missing elapsed time") realToFrac e.elapsed
  get r = concatMap (map secs . snd) $ filter ((== r) . fst) rs


partitionEntries :: [Entry] -> [(DateRange, [Entry])]
partitionEntries es = [(Month, m), (Year, y), (Century, c), (Alltime, nc)]
 where
  (m, nm) = partition (predEntry sameMonth) es
  (y, ny) = partition (predEntry sameYear) nm
  (c, nc) = partition (predEntry sameCentury) ny
  predEntry p e = p (utctDay e.start) e.date
  sameMonth (YearMonthDay _ am _) (YearMonthDay _ bm _) = am == bm
  sameYear (YearMonthDay ay _ _) (YearMonthDay by _ _) = ay == by
  sameCentury (YearMonthDay ay _ _) (YearMonthDay by _ _) = ay `div` 100 == by `div` 100


plotLine :: SaveData -> String
plotLine sd =
  lineGraph
    [ ("Accuracy (last " <> show wSize <> ")", zip [1 ..] line)
    ]
    defPlot
      { plotTitle = "Accuracy over time"
      , xBounds = (Just 1, Just $ c)
      , yBounds = (Just 0, Just 1)
      , xNumTicks = if even c' then 10 else 8
      , xFormatter = \_ _ x -> show @Int $ floor x
      }
 where
  wSize = 7
  es = allEntries sd
  c = fromIntegral c'
  c' = length es
  lengthG = fromIntegral . length
  avg s = if null s then 0 else lengthG (filter isCorrect s) / lengthG s
  line = windowOf wSize avg es


windowOf :: Int -> ([a] -> b) -> [a] -> [b]
windowOf windowSize f ds = reverse . snd $ foldl' window ([], []) (reverse ds)
 where
  window (w, acc) d = let w' = take windowSize (d : w) in (w', f w' : acc)
