{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.Time.Doomsday.Explanation (
    Explanation (..),
    Part (..),
    Step (..),
    -- * Build explanation
    PartBuilder,
    part,
    startingWithYear,
    startingWithDate,
    step,
    stepI,
    note,
    noteI,
    -- * Evaluate explanation
    evalExplanation,
    evalExplanationS,
) where

import Data.List (intercalate, singleton)
import Data.Time.Doomsday.Date
import Data.Time.Doomsday.DayOfWeek (DayOfWeek)
import Data.Time.Doomsday.Equation
import Data.Time.Doomsday.Expression
import Data.Time.Doomsday.State.Simple
import Data.Time.Doomsday.String.Pretty
import Data.Time.Doomsday.Year

---------------------------------------------------------------------
-- DATA
---------------------------------------------------------------------

data Explanation = Explanation
  { parts :: [Part]
  , relativeTo :: Maybe Ordering
  , result :: Maybe DayOfWeek
  }

data Part = Part
  { goal :: String
  , startDate :: StartDate
  , steps :: [Step]
  }

data StartDate
  = StartingWithYear Char (Maybe Year)
  | StartingWithDate Char Char (Maybe Date)

data Step = Step
  { description :: String
  , variable :: Char
  , variableType :: VarType
  , equation :: Equation
  }

data VarType = VDay | VInt

data PartBuilder 
  = PartStartYear Char (State Part Expression)
  | PartStartDate Char Char (State Part Expression)

---------------------------------------------------------------------
-- Pretty
---------------------------------------------------------------------

instance Pretty Explanation where
  pretty expl = intercalate "\n" $ map pretty expl.parts <> res
   where
    res = case (expl.relativeTo, expl.result) of
      (Just o, Just r) -> singleton . unwords $ ["The weekday", tense o, pretty r]
      _ -> []
    tense = \case
      LT -> "was"
      EQ -> "is"
      GT -> "will be"

instance Pretty Part where
  pretty (Part g s ss) = unlines $ (g <> " " <> pretty s) : map ((" - " <>) . pretty) ss

instance Pretty StartDate where
  pretty :: StartDate -> String
  pretty = \case
    StartingWithYear y n -> "Starting with year " <> maybe [y] pretty n <> ":"
    StartingWithDate d _o mDate -> "Starting with date " <> maybe [d] pretty mDate <> ":"

instance Pretty Step where
  pretty :: Step -> String
  pretty s = unwords [s.description, pretty s.equation]

---------------------------------------------------------------------
-- Builder
---------------------------------------------------------------------

part :: String -> PartBuilder -> State Explanation Expression
part goal = \case
  (PartStartYear y (State f)) -> State $ \expl ->
    case f (Part goal (StartingWithYear y Nothing) []) of
      (p, e) -> (expl { parts = rs p : parts expl }, e)
  (PartStartDate d o (State f)) -> State $ \expl ->
    case f (Part goal (StartingWithDate d o Nothing) []) of
      (p, e) -> (expl { parts = rs p : parts expl }, e)
 where
  rs p = p { steps = reverse p.steps }

startingWithYear :: Char -> (Expression -> State Part Expression) -> PartBuilder
startingWithYear y b = PartStartYear y $ b (EVar y)

startingWithDate :: Char -> Char -> (Expression -> Expression -> State Part Expression) -> PartBuilder
startingWithDate d o b = PartStartDate d o $ b (EVar d) (EVar o)

step :: String -> Char -> Expression -> State Part Expression
step = stepG VDay

stepI :: String -> Char -> Expression -> State Part Expression
stepI = stepG VInt

stepG :: VarType -> String -> Char -> Expression -> State Part Expression
stepG typ description variable expression = do
  let s = Step description variable typ (variable ^== expression)
  modify $ \p -> p { steps = s : steps p }
  pure (EVar variable)

note :: String -> Expression -> State Part Expression
note = noteG VDay

noteI :: String -> Expression -> State Part Expression
noteI = noteG VInt

noteG :: VarType -> String -> Expression -> State Part Expression
noteG typ description expression = case expression of
  EVar variable -> do
    let s = Step description variable typ (EqRes expression)
    modify $ \p -> p { steps = s : p.steps }
    pure expression
  _ -> error $ "Expected note to take variable from previous step, instead got: " <> show expression

---------------------------------------------------------------------
-- Evaluation
---------------------------------------------------------------------

type Var = (Char, Expression)

evalExplanation :: Date -> Explanation -> Explanation
evalExplanation d expl = snd . flip runState [] $ evalExplanationS d expl

evalExplanationS :: Date -> Explanation -> State [Var] Explanation
evalExplanationS d expl = do
  nParts <- mapM (evalPart d) expl.parts
  result <- toEnum . eval [] . snd . headVars "explanation" <$> get 
  pure $ expl { parts = nParts, result = Just result }  

evalPart :: Date -> Part -> State [Var] Part
evalPart d p = do
  (pVars, nStart) <- evalStart d p.startDate
  let (nvars, nss) = flip runState pVars $ mapM evalStep p.steps
  let result = headVars "part" nvars
  modify $ (:) result
  pure p { steps = nss, startDate = nStart }

headVars :: String -> [Var] -> Var
headVars n = \case
  (r:_) -> r
  [] -> error $ "cannot eval empty " <> n

evalStart :: Date -> StartDate -> State [Var] ([Var], StartDate)
evalStart d s = do
  vars <- get
  pure $ case s of
    StartingWithYear vy _ -> (year vy : vars, StartingWithYear vy (Just d.year))
    StartingWithDate vd vo _ -> (day vd : doom vo : vars, StartingWithDate vd vo (Just d))
 where
  year vy = (vy, fromIntegral d.year)
  day vd = (vd, fromIntegral d.day)
  doom vo = (vo, error "TODO: closest doomsday")

evalStep :: Step -> State [Var] Step
evalStep s = do
  vars <- get
  let e = s.equation
  let ex = eqResult e
  let sex = substitute vars ex
  let esex = eval [] sex
  let tesex = case s.variableType of VInt -> EConst esex; VDay -> EDay (toEnum esex)
  modify $ (:) (s.variable, tesex)
  pure s { equation = uniq $ e ^== sex ^== esex ^=== tesex }
