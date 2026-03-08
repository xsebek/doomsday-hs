module Data.Time.Doomsday.DayOfWeek (
    DayOfWeek (..),
    daysOfWeek,
) where

data DayOfWeek
    = Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
  deriving (Show, Eq, Ord)

daysOfWeek :: [DayOfWeek]
daysOfWeek = [Sunday .. Saturday]

bimap :: (Int -> Int -> Int) -> DayOfWeek -> DayOfWeek -> DayOfWeek
bimap op x y = toEnum $ fromEnum x `op` fromEnum y

-- | Days of week are numbered starting from Sunday as 0
--   and iteration repeats forever.
instance Enum DayOfWeek where
  toEnum :: Int -> DayOfWeek
  toEnum m = case m `mod` 7 of
    0 -> Sunday
    1 -> Monday
    2 -> Tuesday
    3 -> Wednesday
    4 -> Thursday
    5 -> Friday
    _ -> Saturday
  fromEnum :: DayOfWeek -> Int
  fromEnum = \case
    Sunday -> 0
    Monday -> 1
    Tuesday -> 2
    Wednesday -> 3
    Thursday -> 4
    Friday -> 5
    Saturday -> 6
  enumFrom :: DayOfWeek -> [DayOfWeek]
  enumFrom = map toEnum . enumFrom . fromEnum
  enumFromThen :: DayOfWeek -> DayOfWeek -> [DayOfWeek]
  enumFromThen x y = map toEnum $ enumFromThen (fromEnum x) (fromEnum y)
  enumFromTo :: DayOfWeek -> DayOfWeek -> [DayOfWeek]
  enumFromTo x y = map toEnum $ enumFromTo (fromEnum x) (fromEnum y)
  enumFromThenTo :: DayOfWeek -> DayOfWeek -> DayOfWeek -> [DayOfWeek]
  enumFromThenTo x y z = map toEnum $ enumFromThenTo (fromEnum x) (fromEnum y) (fromEnum z)

instance Num DayOfWeek where
  (+) :: DayOfWeek -> DayOfWeek -> DayOfWeek
  (+) = bimap (+)
  (-) :: DayOfWeek -> DayOfWeek -> DayOfWeek
  (-) = bimap (-)
  (*) :: DayOfWeek -> DayOfWeek -> DayOfWeek
  (*) = bimap (*)
  abs :: DayOfWeek -> DayOfWeek
  abs = id
  signum :: DayOfWeek -> DayOfWeek
  signum = toEnum . signum . fromEnum
  fromInteger :: Integer -> DayOfWeek
  fromInteger = toEnum . fromInteger . (`mod` 7)

instance Real DayOfWeek where
  toRational = toRational . fromEnum

instance Integral DayOfWeek where
  quotRem :: DayOfWeek -> DayOfWeek -> (DayOfWeek, DayOfWeek)
  quotRem x y = let (q,r) = fromEnum x `quotRem` fromEnum y in (toEnum q, toEnum r)
  toInteger :: DayOfWeek -> Integer
  toInteger = toInteger . fromEnum

