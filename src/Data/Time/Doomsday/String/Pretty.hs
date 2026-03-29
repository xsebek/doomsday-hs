{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Doomsday.String.Pretty (
  Pretty(..),
  pretty,
  prettyTerm,
  -- * Formatting pretty text
  Format(..),
  Annotation(..),
  (<+>),
  ($+$),
  default IsString,
  -- * Utils
  tense,
) where

import Data.String ( IsString(..) )
import Data.List (intercalate)

class Pretty a where
  format :: a -> Format
  default format :: Show a => a -> Format
  format = FmtStr . show

pretty :: Pretty a => a -> String
pretty = formatPlain . format

prettyTerm :: Pretty a => a -> String
prettyTerm = formatTerminal termColors . format

instance Pretty Int
instance Pretty Integer

instance Pretty String where
  format = FmtStr

data Format
  = FmtStr String
  | FmtAnn Annotation Format
  | FmtList [Format]
  | FmtConcat [Format]
  | FmtParagraphs [Format]

data Annotation = Math | Failure | Success | Result | Input 

instance Pretty Format where
  format = id

instance IsString Format where
  fromString = FmtStr

default IsString (String, Format)

formatPlain :: Format -> String
formatPlain = formatTerminal (const ("", ""))

type TermColors = Annotation -> (String, String)

termColors :: TermColors
termColors = \case
    Math -> ("\ESC[3m\STX", "\ESC[23m\STX")
    Failure -> c "\ESC[31m\STX"
    Success -> c "\ESC[32m\STX"
    Result -> c "\ESC[33m\STX"
    Input -> c "\ESC[34m\STX"
 where
  -- reset to default foreground color
  c = (,"\ESC[39m\STX")

formatTerminal :: TermColors -> Format -> String
formatTerminal term = \case
  FmtStr s -> s
  FmtAnn a f -> let (s, e) = term a in s <> formatTerminal term f <> e
  FmtList l -> unlines $ mappend " - " . formatTerminal term <$> l
  FmtConcat fs -> mconcat $ formatTerminal term <$> fs
  FmtParagraphs ps -> intercalate "\n" $ formatTerminal term <$> ps

instance Semigroup Format where
  f1 <> f2 | isEmpty f1 = f2
  f1 <> f2 | isEmpty f2 = f1
  (FmtConcat f1) <> (FmtConcat f2) = FmtConcat (f1 <> f2)
  f <> (FmtConcat fs) = FmtConcat (f : fs)
  (FmtConcat fs) <> f = FmtConcat (fs <> [f])
  f1 <> f2 = FmtConcat [f1, f2]  

isEmpty :: Format -> Bool
isEmpty = \case
  FmtStr s -> null s
  FmtAnn _ f -> isEmpty f
  FmtList l -> null l
  FmtConcat fs -> null fs || all isEmpty fs
  FmtParagraphs ps -> null ps

(<+>) :: Format -> Format -> Format
f1 <+> f2 
 | isEmpty f1 = f2
 | isEmpty f2 = f1
 | otherwise = format f1 <> FmtStr " " <> format f2

($+$) :: Format -> Format -> Format
f1 $+$ f2 = format f1 <> FmtStr "\n" <> format f2

tense :: Ordering -> Format
tense = \case
  LT -> "was"
  EQ -> "is"
  GT -> "will be"