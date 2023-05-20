module Ch10 where

myfoldr :: (a->b->b) -> b -> [a] -> b
myfoldr f x [] = x 
myfoldr f b (x:xs) = f x (myfoldr f b xs) 

myscanr :: (a->b->b) -> b -> [a] -> [b]
myscanr f x [] = x : []
myscanr f q (x:xs) = (f x (myfoldr f q xs)) : (myscanr f q xs) 

myfoldl :: (b->a->b) -> b -> [a] -> b
myfoldl f x [] = x
myfoldl f b (x:xs) = myfoldl f (f b x) xs

myscanl :: (a -> b -> a) -> a -> [b] -> [a]
myscanl f q [] = q : []
myscanl f q (x:xs) = q :  myscanl f (f q x) xs


stopvowels :: String -> String -> [String]
stopvowels ss vs = [ [x,y,x'] | x <- ss, y <- vs, x' <- ss]

stopvowelsp :: String -> String -> [String]
stopvowelsp ss vs = filter (\(x:xs) -> x == 'p') $ stopvowels ss vs

nouns :: [String]
nouns = ["cat", "dog", "car", "boy", "girl"]

verbs :: [String]
verbs = ["eats", "kills", "takes", "loves"]

nounsverbs :: [String] -> [String] -> [[String]]
nounsverbs ns vs = [ [xs, ys, xs'] | xs <- ns, ys <- vs, xs' <- ns, xs /= xs']

seekritFunc :: String -> Rational
seekritFunc x = let r = toRational (sum (map length (words x)))
                in (/) r (toRational (length (words x)))

-----------------------------------------------------------------------------------

myOr :: [Bool] -> Bool
myOr [] = False
myOr xs = foldr (||) True xs

myAny :: (a->Bool) -> [a] -> Bool
myAny f = foldr (\x c -> (f x) || c ) False

myElem :: Eq a => a -> [a] -> Bool
myElem e xs = foldr (\x c -> x == e || c) False xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = any (==e)

myReverse :: [a] -> [a]
myReverse = foldl (flip(:)) []  

myMap :: (a->b) -> [a] -> [b]
myMap f  = foldr (\x as -> (f x) : as) []

myFilter :: (a->Bool) -> [a] -> [a]
myFilter f = foldr (\x as -> if (f x) then x : as else as) []

squish :: [[a]] -> [a]
squish = foldr (\xs as -> xs ++ as) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x as -> (f x) ++ as) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\x a -> if compare x a  == GT then x else a) x xs


myMinimumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\x a -> if compare x a  == LT then x else a) x xs

