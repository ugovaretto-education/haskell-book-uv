module Ch9 where
import Data.Char

eftOrd :: (Enum a, Ord a) =>  a -> a -> [a]
eftOrd b e = go b e []
              where go x y xs 
                      | x == y = x : xs
                      | x > y = xs
                      | otherwise = x : (go (succ x) y xs)
 

myWords :: String -> Char -> [String]
myWords s c = go s c []
            where  
              go [] _ xs = xs
              go x c xs = 
                  let x' = takeWhile (/= c) x
                      ys = drop (length x') x
                      ys'= dropWhile (== c) ys
                  in x' : (go ys' c xs)

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

myCubeSqr = [(x,y) | x <- mySqr, y <- myCube]
myCubeSqr50 = [(x,y) | x <- mySqr, y <- myCube, x < 50 && y < 50]
myCubeSqr50Len = length myCubeSqr50

myZip :: [a] -> [b] -> [(a,b)]
myZip xs ys = go xs ys []
              where
                go [] _ xs' = xs'
                go _ [] xs' = xs'
                go (x:xs'') (y:ys'') as = (x,y) : (go xs'' ys'' as)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = go f xs ys []
              where
                go _ [] _ xs' = xs'
                go _ _ [] xs' = xs'
                go f (x:xs'') (y:ys'') as = (f x y) : (go f xs'' ys'' as)

firstCap :: String -> String
firstCap [] = []
firstCap (x:xs) = (toUpper x) : xs

cap :: String -> String
cap [] = []
cap (x:xs) = (toUpper x) : (cap xs) 

capFirst :: String -> Char
capFirst [] = error "Empty string"
capFirst xs = toUpper . head $ xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if (f x) then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = if (e ==  x) then True else myElem e xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (xs : ys) = xs ++ (squish ys) 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs) 

squish2 :: [[a]] -> [a]
squish2 = squishMap id 

myMinBy :: (a -> a -> Ordering) -> [a] -> a
myMinBy _ [x] = x
myMinBy f (x:xs) = let m = myMinBy f xs
                   in if (f x m == LT) then x else m

myMaxBy :: (a -> a -> Ordering) -> [a] -> a
myMaxBy _ [x] = x
myMaxBy f (x:xs) = let m = myMaxBy f xs
                   in if (f x m == GT) then x else m

myMaximum :: Ord a => [a] -> a
myMaximum = myMaxBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinBy compare
