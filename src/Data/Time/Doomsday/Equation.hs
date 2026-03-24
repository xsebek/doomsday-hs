module Data.Time.Doomsday.Equation (
  Equation (..),
  eqResult,
  uniq,

  IsEquation (..),
  (^==),
  (^===),
) where

import Data.Time.Doomsday.Expression
import Data.Time.Doomsday.String.Pretty

data Equation
  = EqRes Expression
  | Expression :== Equation
  | Expression :=== Equation
 deriving (Eq, Show)

infixr 1 :==
infixr 1 :===

instance Pretty Equation where
    pretty :: Equation -> String
    pretty = \case
      EqRes e -> pretty e
      e :== eq -> pretty e <> " = " <> pretty eq
      e :=== eq -> pretty e <> " ≡ " <> pretty eq

eqResult :: Equation -> Expression
eqResult = \case
  EqRes r -> r
  _ :== e -> eqResult e
  _ :=== e -> eqResult e

uniq :: Equation -> Equation
uniq = \case
  e | Just (e1, EqRes e2) <- s e -> if e1 == e2 then EqRes e1 else e
  e1 :== eq1 | Just (e2, eq2) <- s eq1 -> if e1 == e2 then e1 :== uniq eq2 else e1 :== uniq eq1
  e1 :=== eq1 | Just (e2, eq2) <- s eq1 -> if e1 == e2 then e1 :=== uniq eq2 else e1 :=== uniq eq1
  e -> e
 where
  s = \case
    EqRes _ -> Nothing
    e :== eq -> Just (e, eq)
    e :=== eq -> Just (e, eq)

class IsEquation a where
    toEquation :: a -> Equation

instance IsEquation Equation where toEquation = id
instance IsEquation Expression where toEquation = EqRes
instance IsEquation Char where toEquation = EqRes . EVar

(^==) :: (IsEquation a, IsEquation b) => a -> b -> Equation
a ^== b = toEquation a `eqConcat` toEquation b

(^===) :: (IsEquation a, IsEquation b) => a -> b -> Equation
a ^=== b = toEquation a `equivConcat` toEquation b

eqConcat :: Equation -> Equation -> Equation
eqConcat e1 e2 = case e1 of
  EqRes r -> r :== e2
  ex1 :== e -> ex1 :== eqConcat e e2
  ex1 :=== e -> ex1 :=== eqConcat e e2

equivConcat :: Equation -> Equation -> Equation
equivConcat e1 e2 = case e1 of
  EqRes r -> r :== e2
  ex1 :== e -> ex1 :== eqConcat e e2
  ex1 :=== e -> ex1 :=== eqConcat e e2