module Main where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

property_lt :: Int -> Bool
property_lt x =  x - 1 < x

trivialInt :: Gen Int
trivialInt = return 0

-- main :: IO ()
-- main = quickCheck $ forAll trivialInt property_lt
half :: Float -> Float
half x = x / 2

halfIdentity :: Float -> Float
halfIdentity = (* 2) . half

prop_half :: IO ()
prop_half =
  hspec $ do
    describe "Half" $ do
      it "Half / 2 x Half = Half" $ do
        property $ \x -> halfIdentity x == (x :: Float)

prop_halfQC = quickCheck $ forAll (arbitrary :: Gen Float) (\x -> halfIdentity x == x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs
  -- traversing right to left, invariant: xs[i+1] >= xs[i]
 = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

prop_intListOrdered :: IO ()
prop_intListOrdered =
  hspec $ do
    describe "Ordered list" $ do
      it "xs[i+1] >= xs[i] for all indices in list" $ do
        property $ (\xs -> listOrdered . sort $ (xs :: [Int]))

plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

prop_assocInt :: IO ()
prop_assocInt =
  hspec $ do
    describe "Associativity" $ do
      it "(x+y) + z = x + (y + z)" $ do
        property $ (plusAssociative :: Int -> Int -> Int -> Bool)

prop_commInt :: IO ()
prop_commInt =
  hspec $ do
    describe "Commutativity" $ do
      it "x + y = y + x" $ do
        property $ (plusCommutative:: Int -> Int -> Bool)

genFun :: Gen (Int -> Int)
genFun = oneof [return (\x -> 2 * x), return (\x -> x  - 1)]

genComp :: Gen (Int -> Int)
genComp = do
  f <- genFun
  g <- genFun
  return (f . g)

main :: IO ()
main = do
  prop_half
  prop_intListOrdered
  prop_commInt
  prop_assocInt
  return ()
