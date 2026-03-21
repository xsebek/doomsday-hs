{-# LANGUAGE RecordWildCards #-}
module Data.Time.Doomsday.Explanation (
    Explanation (..),
    Part (..),
    Step (..),
    step,
    part,
    startingWithYear,
) where

import Data.Time.Doomsday.Expression
import Data.Time.Doomsday.State.Simple
import Data.Time.Doomsday.String.Pretty
import Data.List (intercalate)

data Explanation = Explanation
  { parts :: [Part]
  }

instance Pretty Explanation where
  pretty :: Explanation -> String
  pretty (Explanation ps) = intercalate "\n" $ map pretty (reverse ps)

data Part = Part
  { goal :: String
  , startDate :: StartDate
  , steps :: [Step]
  }

instance Pretty Part where
  pretty :: Part -> String
  pretty (Part g s ss) = unlines $ (g <> " " <> pretty s) : map ((" - " <>) . pretty) (reverse ss)

data StartDate = StartingWithYear Char | StartingWithDate

instance Pretty StartDate where
  pretty :: StartDate -> String
  pretty = \case
    StartingWithYear y -> "Starting with year " <> [y] <> ":"
    StartingWithDate -> "Starting with date:"

data Step = Step
  { description :: String
  , variable :: Char
  , expression :: Expression
  }

instance Pretty Step where
  pretty :: Step -> String
  pretty (Step d v e) = unwords [d, [v], "=", pretty e]

step :: String -> Char -> Expression -> State Part Expression
step description variable expression = State $ \p ->
  (p { steps = Step description variable expression : steps p }, EVar variable)

data PartBuilder
  = PartStartYear Char (State Part Expression)

startingWithYear :: Char -> (Expression -> State Part Expression) -> PartBuilder
startingWithYear y b = PartStartYear y $ b (EVar y)

part :: String -> PartBuilder -> State Explanation Expression
part goal = \case
  (PartStartYear y (State f)) -> State $ \expl ->
    case f (Part goal (StartingWithYear y) []) of
      (p, e) -> (expl { parts = p : parts expl }, e)
