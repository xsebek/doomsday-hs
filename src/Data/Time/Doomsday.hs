module Data.Time.Doomsday (
  doomsdayExplanation,
  findCenturyAnchor,
  findYearAnchor,
  findWeekday,
  
  module D,
  module Y,
  module M,
  module W,
  module Expression,
  module Equation,
  module Explanation,

  Pretty(..),
  State (..),
) where

import Data.Time.Doomsday.Date as D
import Data.Time.Doomsday.Year as Y
import Data.Time.Doomsday.Month as M
import Data.Time.Doomsday.DayOfWeek as W
import Data.Time.Doomsday.Equation as Equation
import Data.Time.Doomsday.Explanation as Explanation
import Data.Time.Doomsday.Expression as Expression
import Data.Time.Doomsday.State.Simple (State (..))
import Data.Time.Doomsday.String.Pretty as Pretty

doomsdayExplanation :: Explanation
doomsdayExplanation = explanation $ do
  a <- findCenturyAnchor
  w <- findYearAnchor a
  findWeekday w

findCenturyAnchor :: State Explanation Expression
findCenturyAnchor =
  part "Find the century anchor." $ startingWithYear 'Y' $ \y -> do
    c <- stepI "take the century digits" 'C' $ y `div` 100
    f <- stepI "in a four century cycle its index is" 'F' $ c `mod` 4
    i <- step "the resulting increment is" 'I' $ f * 5
    step "add Tuesday and get" 'A' $ EDay Tuesday + i

findYearAnchor :: Expression -> State Explanation Expression
findYearAnchor centuryAnchor =
  part "Find the year anchor." $ startingWithYear 'Y' $ \y -> do
    a <- note "note the century anchor" centuryAnchor
    t <- stepI "take the last two digits" 'T' $ y `mod` 100
    i <- step "the resulting increment is" 'I' $ t + t `div` 4
    step "add the century anchor to get" 'W' $ a + i

findWeekday :: Expression -> State Explanation Expression
findWeekday yearAnchor =
  part "Find the weekday." $ startingWithDate 'D' 'O' $ \date doom -> do
    w <- note "note the year anchor" yearAnchor
    o <- noteI "this months doomsday is" doom
    i <- step "the resulting increment is" 'I' $ date - o
    step "add the year anchor to get" 'R' $ w + i
