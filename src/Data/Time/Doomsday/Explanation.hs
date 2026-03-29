{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Time.Doomsday.Explanation (
    Explanation (..),
    Part (..),
    Step (..),
    -- * Build explanation
    explanation,
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

import Data.Time.Doomsday.Date
import Data.Time.Doomsday.DayOfWeek (DayOfWeek)
import Data.Time.Doomsday.Equation
import Data.Time.Doomsday.Expression
import Data.Time.Doomsday.State.Simple
import Data.Time.Doomsday.String.Pretty
import Data.Time.Doomsday.Year
import Data.Time.Doomsday.Mnemonic (closestDoomsday, defaultMnemonics)
import Data.Bool (bool)

---------------------------------------------------------------------
-- DATA
---------------------------------------------------------------------

data Explanation = Explanation
  { parts :: [Part]
  , relativeTo :: Maybe Ordering
  , result :: Maybe DayOfWeek
  , correct :: Maybe Bool
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
  format expl = FmtParagraphs $ map format expl.parts <> res
   where
    res = case (expl.relativeTo, expl.result) of
      (Just o, Just r) -> [guess <> "The weekday" <+> tense o <+> FmtAnn Result (format r) <> "."]
      _ -> []
    guess = maybe "" ((<> " ") . bool (FmtAnn Failure "Wrong!") (FmtAnn Success "Correct!")) expl.correct
    tense = \case
      LT -> "was"
      EQ -> "is"
      GT -> "will be"

instance Pretty Part where
  format (Part g s ss) = FmtStr g <+> format s $+$ FmtList (map format ss)

instance Pretty StartDate where
  format = \case
    StartingWithYear y n -> "Starting with year" <+> inputFmt y n <> ":"
    StartingWithDate d _o mDate -> "Starting with date" <+> inputFmt d mDate <> ":"
   where
    inputFmt :: Pretty a => Char -> Maybe a -> Format
    inputFmt var f = FmtAnn Input $ maybe (FmtStr [var]) format f

instance Pretty Step where
  format s = format s.description <+> format s.equation

---------------------------------------------------------------------
-- Builder
---------------------------------------------------------------------

explanation :: State Explanation a -> Explanation
explanation sExpl = r . fst . runState sExpl $ Explanation [] Nothing Nothing Nothing
 where
  r e = e { parts = reverse e.parts }

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
  doom vo = (vo, fromIntegral $ (snd $ closestDoomsday defaultMnemonics d).day)

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
