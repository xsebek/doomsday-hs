module Data.Time.Doomsday (
  findCenturyAnchor,
  
  module D,
  module Y,
  module M,
  module W,
  module Expression,
  module Explanation,

  Pretty(..),
  State (..),
) where

import Data.Time.Doomsday.Date as D
import Data.Time.Doomsday.Year as Y
import Data.Time.Doomsday.Month as M
import Data.Time.Doomsday.DayOfWeek as W
import Data.Time.Doomsday.Explanation as Explanation
import Data.Time.Doomsday.Expression as Expression
import Data.Time.Doomsday.State.Simple (State (..))
import Data.Time.Doomsday.String.Pretty as Pretty

findCenturyAnchor :: State Explanation Expression
findCenturyAnchor =
  part "Find the century anchor." $ startingWithYear 'Y' $ \y -> do
    c <- step "take the century digits" 'C' $ y `div` 100
    f <- step "in a four century cycle its index is" 'F' $ c `mod` 4
    i <- step "the resulting increment is" 'I' $ f * 5
    step "add Tuesday and get" 'A' $ EDay Tuesday + i
