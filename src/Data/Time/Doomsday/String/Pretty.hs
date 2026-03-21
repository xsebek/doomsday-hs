{-# LANGUAGE DefaultSignatures #-}
module Data.Time.Doomsday.String.Pretty where

class Pretty a where
    pretty :: a -> String

    default pretty :: Show a => a -> String
    pretty = show