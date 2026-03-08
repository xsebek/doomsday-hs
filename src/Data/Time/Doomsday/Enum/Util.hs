module Data.Time.Doomsday.Enum.Util (
    iso1,
    iso2,
    iso3,
    iso2P,
    enumFromToEq,
    enumFromThenToEqZeroBased,
    Pred (..),
) where

iso1 :: Enum a => (Int -> Int) -> a -> a
iso1 op = toEnum . op .  fromEnum

iso2 :: Enum a => (Int -> Int -> Int) -> a -> a -> a
iso2 op x y = toEnum $ fromEnum x `op` fromEnum y

iso3 :: Enum a => (Int -> Int -> Int -> Int) -> a -> a -> a -> a
iso3 op x y z = toEnum $ op (fromEnum x) (fromEnum y) (fromEnum z)

iso2P :: Enum a => (Int -> Int -> (Int, Int)) -> a -> a -> (a, a)
iso2P op x y = let (a, b) = fromEnum x `op` fromEnum y in (toEnum a, toEnum b)

enumFromToEq :: (Eq a, Enum a) => a -> a -> [a]
enumFromToEq start end
  | start == end = [start]
  | otherwise = start : enumFromTo (succ start) end

enumFromThenToEqZeroBased :: (Enum a, Eq a) => a -> a -> a -> [a]
enumFromThenToEqZeroBased start next end
  | next == end = [start, end]
  | otherwise = start : enumFromThenToEqZeroBased next next2 end
 where
  next2 = toEnum $ (2 * fromEnum next) - (fromEnum start)

newtype Pred e = Pred { getPred :: e }
  deriving (Eq, Ord)

instance Enum e => Enum (Pred e) where
  toEnum :: Enum e => Int -> Pred e
  toEnum = Pred . toEnum . succ
  fromEnum :: Enum e => Pred e -> Int
  fromEnum = pred . fromEnum . getPred
