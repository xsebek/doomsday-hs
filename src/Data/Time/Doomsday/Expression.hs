{-# LANGUAGE RecordWildCards #-}

module Data.Time.Doomsday.Expression (
  Expression (..),
  eval,
  pretty,
) where

import Data.Time.Doomsday.DayOfWeek
import Data.Foldable (find)

data Expression
  = EConst Int
  | EDay DayOfWeek
  | EVar Char
  | EAdd Expression Expression
  | ENeg Expression
  | EMul Expression Expression
  | EDiv Expression Expression
  | EMod Expression Expression
 deriving (Eq, Ord, Show)

eval :: [(Char, Int)] -> Expression -> Int
eval vars = \case
  EConst i -> i
  EDay d -> fromEnum d
  EVar v -> case find ((== v) . fst) vars of
    Nothing -> error $ "Unknown variable: " <> [v]
    Just (_, i) -> i
  ENeg e1 -> - eval vars e1
  EAdd e1 e2 -> eval vars e1 + eval vars e2
  EMul e1 e2 -> eval vars e1 * eval vars e2
  EDiv e1 e2 -> eval vars e1 `div` eval vars e2
  EMod e1 e2 -> eval vars e1 `mod` eval vars e2

pretty :: Expression -> String
pretty = go 12
 where
  b :: Int -> Int -> String -> String
  b pOut pIn s = if pOut <= pIn then "(" <> s <> ")" else s
  go :: Int -> Expression -> String
  go p = \case
    EConst i -> show i
    EDay d -> show d
    EVar v -> [v]
    ENeg e1 -> b p 4 $ "-" <> go 4 e1
    EAdd e1 (ENeg e2) -> b p 4 $ go 4 e1 <> " - " <> go 4 e2    
    EAdd e1 e2 -> b p 4 $ go 4 e1 <> " + " <> go 4 e2
    EMul e1 e2 -> b p 3 $ go 3 e1 <> " * " <> go 3 e2
    EDiv e1 e2 -> b p 3 $ go 3 e1 <> " / " <> go 3 e2
    EMod e1 e2 -> b p 3 $ go 3 e1 <> " % " <> go 3 e2

instance Num Expression where
  (+) :: Expression -> Expression -> Expression
  (+) = EAdd
  (*) :: Expression -> Expression -> Expression
  (*) = EMul
  negate :: Expression -> Expression
  negate = ENeg
  fromInteger :: Integer -> Expression
  fromInteger = EConst . fromInteger
  abs :: Expression -> Expression
  abs = error "abs is not supported for expressions"
  signum :: Expression -> Expression
  signum = error "signum is not supported for expressions"

instance Real Expression where
  toRational :: Expression -> Rational
  toRational = fromIntegral . eval []

instance Enum Expression where
  toEnum :: Int -> Expression
  toEnum = EConst
  fromEnum :: Expression -> Int
  fromEnum = eval []

instance Integral Expression where
  div :: Expression -> Expression -> Expression
  div = EDiv
  mod :: Expression -> Expression -> Expression
  mod = EMod
  quot :: Expression -> Expression -> Expression
  quot = error "quot is not supported for expressions"
  rem :: Expression -> Expression -> Expression
  rem = error "rem is not supported for expressions"
  quotRem :: Expression -> Expression -> (Expression, Expression)
  quotRem = error "quotRem is not supported for expressions"
  toInteger :: Expression -> Integer
  toInteger = toInteger . eval []
