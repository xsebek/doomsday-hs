{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Doomsday.Equation (
  Equation (..),
  eqResult,
  uniq,

  IsEquation (..),
  (^==),
  (^===),
  IsFinal (..),
) where

import Data.Time.Doomsday.Expression
import Data.Time.Doomsday.String.Pretty
import Data.Time.Doomsday.DayOfWeek (DayOfWeek)

data Equation
  = EqRes Expression
  | Expression :== Equation
  | Expression :=== Equation
 deriving (Eq, Show)

infixr 1 :==
infixr 1 :===

data IsFinal a = IsFinal Bool a

instance Pretty (IsFinal Equation) where
  format (IsFinal fin equa) = FmtAnn Math $ fmt equa
   where
    fmt = \case
      EqRes e -> (if fin then FmtAnn Result else id) $ format e
      e :== eq -> format e <+> "=" <+> fmt eq
      e :=== eq -> format e <+> "≡" <+> fmt eq


type EqOp = Expression -> Equation -> Equation

eqToEither :: Equation -> Either (Expression, EqOp, Equation) Expression
eqToEither = \case
  EqRes r -> Right r
  e :== eq -> Left (e, (:==), eq) 
  e :=== eq -> Left (e, (:===), eq) 

eqResult :: Equation -> Expression
eqResult eq = case eqToEither eq of
  Right r -> r
  Left (_, _, e) -> eqResult e

uniq :: Equation -> Equation
uniq = \case
  e | Left (e1, _, EqRes e2) <- eqToEither e
        -> if isSame e1 e2 then EqRes e1 else e
    | Left (e1, eOp1, eq2) <- eqToEither e
    , Left (e2, eOp2, eq3) <- eqToEither eq2
        -> if isSame e1 e2 then uniq $ e1 `eOp2` eq3 else e1 `eOp1` uniq eq2
    | otherwise
        -> e

isSame :: Expression -> Expression -> Bool
isSame (EDay d) (EConst i) = fromEnum d == i
isSame e1 e2 = e1 == e2

class IsEquation a where
    toEquation :: a -> Equation

instance IsEquation Equation where toEquation = id
instance IsEquation Expression where toEquation = EqRes
instance IsEquation Int where toEquation = EqRes . EConst
instance IsEquation Char where toEquation = EqRes . EVar
instance IsEquation DayOfWeek where toEquation = EqRes . EDay

(^==) :: (IsEquation a, IsEquation b) => a -> b -> Equation
a ^== b = toEquation a `eqConcat` toEquation b

(^===) :: (IsEquation a, IsEquation b) => a -> b -> Equation
a ^=== b = toEquation a `equivConcat` toEquation b

infixr 4 ^==
infixr 4 ^===

eqConcat :: Equation -> Equation -> Equation
eqConcat e1 e2 = case eqToEither e1 of
  Right r -> r :== e2
  Left (ex, eOp, eq) -> ex `eOp` (eqConcat eq e2)

equivConcat :: Equation -> Equation -> Equation
equivConcat e1 e2 = case eqToEither e1 of
  Right r -> r :=== e2
  Left (ex, eOp, eq) -> ex `eOp` (eqConcat eq e2)