{-# LANGUAGE DuplicateRecordFields #-}

module Data.Time.Doomsday (
  -- * Conway's Doomsday algorithm
  doomsdayExplanation,
  findCenturyAnchor,
  findYearAnchor,
  findWeekday,
  -- * Custom algorithm versions
  -- ** Simpler formula with division by 4
  doomsdayExplanationDiv4,
  findYearAnchorDiv4,
  -- ** Odd + 11 method
  doomsdayExplanationOdd11,
  findYearAnchorOdd11,

  -- * How to create an explanation
  module Expression,
  module Equation,
  module Explanation,
  -- ** Mnemonics
  module Mnemonic,
  -- ** Calendar
  module D,
  module Y,
  module M,
  module W,
  -- ** Simple state
  State (..),
  -- ** Simple pretty printing
  module Pretty,
) where

import Data.Time.Doomsday.Date as D
import Data.Time.Doomsday.DayOfWeek as W
import Data.Time.Doomsday.Equation as Equation
import Data.Time.Doomsday.Explanation as Explanation
import Data.Time.Doomsday.Expression as Expression
import Data.Time.Doomsday.Mnemonic as Mnemonic
import Data.Time.Doomsday.Month as M
import Data.Time.Doomsday.State.Simple (State (..))
import Data.Time.Doomsday.String.Pretty as Pretty
import Data.Time.Doomsday.Year as Y


---------------------------------------------------------------------
-- Original Conway's algorithm
---------------------------------------------------------------------

doomsdayExplanation :: Explanation
doomsdayExplanation = explanation $ do
  a <- findCenturyAnchor
  w <- findYearAnchor a
  findWeekday w


findCenturyAnchor :: State Explanation Expression
findCenturyAnchor =
  part "Find the century anchor." $ startingWithYear 'Y' $ \y -> do
    c <- stepI "take the century digits" $ 'C' := y `div` 100
    f <- stepI "in a four century cycle its index is" $ 'F' := c `mod` 4
    i <- step "the resulting increment is" $ 'I' := f * 5
    step "add Tuesday and get" $ 'A' := EDay Tuesday + i


findYearAnchor :: Expression -> State Explanation Expression
findYearAnchor centuryAnchor =
  part "Find the year anchor." $ startingWithYear 'Y' $ \y -> do
    a <- note "note the century anchor" centuryAnchor
    t <- stepI "take the last two digits" $ 'T' := y `mod` 100
    d <- stepI "divide them by twelve" $ 'D' := t `div` 12
    r <- stepI "and take the reminder" $ 'R' := t `mod` 12
    i <- step "the resulting increment is" $ 'I' := d + r + (r `div` 4)
    step "add the century anchor to get" $ 'W' := a + i


findWeekday :: Expression -> State Explanation Expression
findWeekday yearAnchor =
  part "Find the weekday." $ startingWithDate 'D' 'O' $ \date doom -> do
    w <- note "note the year anchor" yearAnchor
    o <- noteI "this months doomsday is" doom
    i <- step "the resulting increment is" $ 'I' := date - o
    step "add the year anchor to get" $ 'R' := w + i


---------------------------------------------------------------------
-- Custom
---------------------------------------------------------------------

doomsdayExplanationDiv4 :: Explanation
doomsdayExplanationDiv4 = explanation $ findCenturyAnchor >>= findYearAnchorDiv4 >>= findWeekday


-- | The Conways method for computing the years doomsday is equivalent
-- to this simpler formula, which requires dividing by both 4 and 7.
--
-- See https://en.wikipedia.org/wiki/Doomsday_rule#Why_it_works
findYearAnchorDiv4 :: Expression -> State Explanation Expression
findYearAnchorDiv4 centuryAnchor =
  part "Find the year anchor." $ startingWithYear 'Y' $ \y -> do
    a <- note "note the century anchor" centuryAnchor
    t <- stepI "take the last two digits" $ 'T' := y `mod` 100
    i <- step "the resulting increment is" $ 'I' := t + t `div` 4
    step "add the century anchor to get" $ 'W' := a + i


doomsdayExplanationOdd11 :: Explanation
doomsdayExplanationOdd11 = explanation $ findCenturyAnchor >>= findYearAnchorOdd11 >>= findWeekday

-- | The odd plus 11 method by Chamberlain Fong and Michael K. Walters.
--
-- See _Methods for Accelerating Conway's Doomsday Algorithm_
-- https://doi.org/10.48550/arXiv.1010.0765
findYearAnchorOdd11 :: Expression -> State Explanation Expression
findYearAnchorOdd11 centuryAnchor =
  part "Find the year anchor." $ startingWithYear 'Y' $ \y -> do
    a <- note "note the century anchor" centuryAnchor
    t1 <- stepI "take the last two digits" $ 'T' := y `mod` 100
    t2 <- stepI "if odd, add eleven" $ 'T' := t1 + (t1 `mod` 2) * 11
    t3 <- stepI "then divide by two" $ 'T' := t2 `div` 2
    t4 <- stepI "if odd, add eleven" $ 'T' := t3 + (t3 `mod` 2) * 11
    t5 <- step "subtract from seven" $ 'T' := 7 - t4
    step "add the century anchor to get" $ 'W' := a + t5
