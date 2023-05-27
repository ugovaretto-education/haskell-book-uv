module Scratch where

import Data.Char
import qualified Data.Map as M

-- data Car = MkCar {company :: String, model :: String, year :: Int} deriving (Show)
data Car a b c =
  Car
    { company :: a
    , model :: b
    , year :: c
    }
  deriving (Show)

type Digit = Char

type Passes = Int

-- capital and number handled separately
type CharPasses = M.Map Char (Digit, Passes)

data DaPhone =
  DaPhone CharPasses
  deriving (Show)

phone =
  DaPhone
    (M.fromList
       [ ('#', ('#', 3))
       , ('.', ('#', 1))
       , (',', ('#', 2))
       , ('0', ('0', 3))
       , ('+', ('0', 1))
       , (' ', ('0', 2))
       , ('1', ('1', 1))
       , ('2', ('2', 4))
       , ('a', ('2', 1))
       , ('b', ('2', 2))
       , ('c', ('2', 3))
       , ('3', ('3', 4))
       , ('d', ('3', 1))
       , ('e', ('3', 2))
       , ('f', ('3', 3))
       , ('4', ('4', 4))
       , ('g', ('4', 1))
       , ('h', ('4', 2))
       , ('i', ('4', 3))
       , ('5', ('5', 4))
       , ('j', ('5', 1))
       , ('k', ('5', 2))
       , ('l', ('5', 3))
       , ('6', ('6', 4))
       , ('m', ('6', 1))
       , ('n', ('6', 2))
       , ('o', ('6', 3))
       , ('7', ('7', 5))
       , ('p', ('7', 1))
       , ('q', ('7', 2))
       , ('r', ('7', 3))
       , ('s', ('7', 4))
       , ('8', ('8', 4))
       , ('t', ('8', 1))
       , ('u', ('8', 2))
       , ('v', ('8', 3))
       , ('9', ('9', 5))
       , ('w', ('9', 1))
       , ('x', ('9', 2))
       , ('y', ('9', 3))
       , ('z', ('9', 4))
       ])

charsToPasses :: DaPhone -> Char -> String
charsToPasses p@(DaPhone m) c
  | isLower c || c == ' ' || c == '.' || c == ',' || c == '+' =
    case M.lookup c m of
      Just (d, p) -> replicate p d
      _ -> ""
  | otherwise = '*' : (charsToPasses p (toLower c))

charsToDigits :: DaPhone -> String -> String
charsToDigits d xs = foldr (++) "" $ map (charsToPasses d) xs

histogram :: (Ord a) => [a] -> M.Map a Int
histogram xs = hist xs M.empty
  where
    hist :: (Ord a2) => [a2] -> M.Map a2 Int -> M.Map a2 Int
    hist [] m = m
    hist (x:xs) m =
      case M.member x m of
        True -> hist xs (M.adjust (+ 1) x m)
        _ -> hist xs (M.insert x 1 m)

maxInMap :: (Ord a) => (a, Int) -> M.Map a Int -> (a, Int)
maxInMap start m =
  M.foldrWithKey
    (\k v (k', v') ->
       if v' > v
         then (k', v')
         else (k, v))
    start
    m

minInMap :: (Ord a) => (a, Int) -> M.Map a Int -> (a, Int)
minInMap start m =
  M.foldrWithKey
    (\k v (k', v') ->
       if v' < v
         then (k', v')
         else (k, v))
    start
    m

maxInPairs :: (Ord b) => (a, b) -> [(a, b)] -> (a, b)
maxInPairs _ [] = error "Empty list"
maxInPairs start xs =
  foldr
    (\(x, y) (x', y') ->
       if y' > y
         then (x', y')
         else (x, y))
    start
    xs

minInPairs :: (Ord b) => (a, b) -> [(a, b)] -> (a, b)
minInPairs _ [] = error "Empty list"
minInPairs start xs =
  foldr
    (\(x, y) (x', y') ->
       if y' < y
         then (x', y')
         else (x, y))
    start
    xs

------------------------------------------------------------------------------
------------------------------------------------------------------------------
data Expr
  = Lit Integer
  | Add Expr Expr
  deriving (Show)

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add ex1 ex2) = (eval ex1) + (eval ex2)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add ex1 ex2) = (printExpr ex1) ++ " + " ++ (printExpr ex2)
