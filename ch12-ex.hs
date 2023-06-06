module Ch123 where

import Data.Char

data AWord a
  = IndetArticle a
  | DetArticle a
  | NotArticle a
  deriving (Show)

data Vowel =
  Vowel Char

data Consonant =
  Consonant Char

mkVowel :: Char -> Maybe Vowel
mkVowel x =
  if elem x "aeiou"
    then Just (Vowel x)
    else Nothing

mkConsonant :: Char -> Maybe Consonant
mkConsonant x =
  if elem x "aeiou"
    then Just (Consonant x)
    else Nothing

mkWord :: String -> AWord String
mkWord [] = IndetArticle ""
mkWord "the" = DetArticle "the"
mkWord "The" = DetArticle "The"
mkWord "A" = IndetArticle "A"
mkWord "a" = IndetArticle "a"
mkWord x = NotArticle x

replaceThe :: String -> [String]
replaceThe [] = []
replaceThe xs =
  let xss = words xs
   in map
        (\x ->
           case x of
             "the" -> "a"
             "The" -> "A"
             _ -> x)
        xss

mapWord' :: AWord String -> String
mapWord' x =
  case x of
    DetArticle (x:xs) -> (toUpper x) : xs
    IndetArticle s -> s
    NotArticle t -> t

splitWords :: String -> [String]
splitWords xs = splitWords' xs "" []

splitWords' :: String -> String -> [String] -> [String]
splitWords' "" y xs = xs ++ [y]
splitWords' (x:xs) y xss
  | x == ' ' && y /= "" = splitWords' xs "" (xss ++ [y])
  | x == ' ' && y == "" = splitWords' xs "" xss
  | otherwise = splitWords' xs (y ++ [x]) xss

theVowel :: [String] -> Int
theVowel [] = 0
theVowel (x:[]) = 0
theVowel (x:(y:ys):xss) =
  let isVow = elem y "aeiou"
   in if isVow && (x == "the")
        then 1 + (theVowel xss)
        else theVowel xss

numVowels :: String -> Int
numVowels [] = 0
numVowels (x:xs) =
  if x `elem` "aeiou"
    then 1 + (numVowels xs)
    else numVowels xs

numVowels' :: String -> Int
numVowels' =
  foldr
    (\x a ->
       if elem x "aeiou"
         then 1 + a
         else a)
    0

countVC :: String -> (Int, Int)
countVC "" = (0, 0)
countVC (x:xs) =
  let (v, c) = countVC xs
      incC =
        if (elem x ['a' .. 'z'] && not (elem x "aeiou"))
          then 1
          else 0
      incV =
        if elem x "aeiou"
          then 1
          else 0
   in (v + incV, c + incC)

newtype Word' =
  Word' String

mkWord' :: String -> Maybe Word'
mkWord' "" = Nothing
mkWord' xs =
  let (v, c) = countVC xs
   in if v > c
        then Nothing
        else Just (Word' xs)

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

intToNat :: Integer -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n - 1))

-- Maybe
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

catMaybes :: (Eq a) => [Maybe a] -> [a]
catMaybes = map (\(Just x) -> x) . filter (/= Nothing)

flipMaybe :: (Eq a) => [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs =
  if (elem Nothing xs)
    then Nothing
    else Just (catMaybes xs)

-- Either
lefts' :: [Either a b] -> [a]
lefts' =
  foldr
    (\x xs ->
       case x of
         Left y -> xs ++ [y]
         _ -> xs)
    []

rights' :: [Either a b] -> [b]
rights' =
  foldr
    (\x xs ->
       case x of
         Right y -> xs ++ [y]
         _ -> xs)
    []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (\x -> Just (f x))

-- Unfold
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

fib :: Int -> Int -> [Int]
fib x y = (x + y) : (fib y (x + y))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case (f x) of
    Nothing -> []
    Just (x', y') -> x' : myUnfoldr f y'

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree f n = case f n of
                  Nothing -> Leaf
                  Just (l, x, r) -> Node (unfoldTree f l) x (unfoldTree f r)  


treeBuilder :: Integer -> BinaryTree Integer
treeBuilder i = unfoldTree (\n -> if n == i then Nothing else Just (n+1, n, n+1)) 0 

printTree :: (Show a) => BinaryTree a -> IO ()
printTree Leaf = print ""
printTree (Node l a r) = do 
                          putStr "\t"
                          putStrLn (show a)
                          putStr "\t"
                          printTree l
                          printTree r


testTree'' :: BinaryTree Integer
testTree'' =
        Node 
          (Node
            (Node Leaf 3 Leaf)
            1 
            (Node Leaf 10 Leaf))
        2
          (Node
            (Node Leaf 35 Leaf)
            11
            (Node Leaf 110 Leaf))
