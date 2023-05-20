module Ch6 where

data StringOrInt
  = TisAnInt Int
  | TisAString String
  deriving (Show)

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) (TisAString x) (TisAString x') = x == x'
  (==) _ _ = False

data Pair a = Pair a a
  deriving (Show)

instance Eq a => Eq (Pair a) where
  (==) (Pair x x') (Pair y y') = x == y && x' == y'

data Which a
  = ThisOne a
  | ThatOne a
  deriving (Show)

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

newtype Age
  = Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber = Age
  toNumber (Age n) = n

newtype Year
  = Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber = Year
  toNumber (Year n) = n

sumNumberish :: (Numberish a, Numberish b) => a -> b -> a
sumNumberish a a' = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfAPrime = toNumber a'
    summed =
      integerOfA + integerOfAPrime

instance Numberish Integer where
  fromNumber n = n
  toNumber n = n
