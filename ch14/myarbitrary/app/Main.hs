module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Identity a =
  Identity a
  deriving (Eq, Show)

-- identityGen :: Arbitrary a => Gen (Identity a)
-- identityGen = do
--         a <- arbitrary
--         return (Identity a)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = arbitrary  >>= (\x -> return (Identity x ))
data Pair a b = Pair a b deriving (Eq, Show)
pairGen :: (Arbitrary a,
            Arbitrary b) => Gen (Pair a b)
-- pairGen = do
--   a <- arbitrary
--   b <- arbitrary
--   return (Pair a b)
pairGen = arbitrary >>= (\x -> arbitrary >>= (\y -> return (Pair x y)))

instance (Arbitrary a,
          Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen
data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)
-- equal odds for each
sumGenEqual :: (Arbitrary a, Arbitrary b) =>
                  Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

main :: IO ()
main = putStrLn "Hello, Haskell!"
