{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Ch11 where

data Vehicle = Car Manufacturer Price |
               Plane Airline PlaneSize
               deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Price = Price Int deriving (Eq, Show)
data PlaneSize = PlaneSize Int deriving (Eq, Show)
data Airline = PapuAir | CatapultR'us | TakeYourChanceUnited
               deriving (Eq, Show)
myCar= Car Mini (Price 14000)
urCar= Car Mazda (Price 20000) 
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "Not a car"

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Integer, String) where
  tooMany (x,s) = (x + (read s :: Integer)) > 42

instance TooMany Int where
  tooMany n = n > 42

instance (Num a, TooMany a) => TooMany (a,a) where
    tooMany (x, y) = tooMany x  || tooMany y

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
-- instance TooMany Goats where
--   tooMany (Goats n) = n > 43
--
-- 11.13

data GuessWhat = ChickenButt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a
                                       , psecond :: b }
                                       deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
data Animal = Cow CowInfo
              | Pig PigInfo
              | Sheep SheepInfo
              deriving (Eq, Show)
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
idInt :: Id Integer
idInt = MkId 10
type Awesome = Bool
person :: Product Name Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)
socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct { pfirst = 42
                          , psecond = 0.00001 }

data OperatingSystem =
                      GnuPlusLinux
                      | OpenBSDPlusNevermindJustBSDStill
                      | Mac
                      | Windows
                      deriving (Eq, Show)
data ProgLang =
                Haskell
                | Agda
                | Idris
                | PureScript
                deriving (Eq, Show)
data Programmer =
                  Programmer { os :: OperatingSystem
                             , lang :: ProgLang }
                  deriving (Eq, Show)               

-- Write a function that generates all possible values of Programmer. Use
-- the provided lists of inhabitants of OperatingSystem and ProgLang:
nextOS :: Maybe OperatingSystem -> Maybe OperatingSystem
nextOS Nothing = Just GnuPlusLinux
nextOS (Just GnuPlusLinux) = Just OpenBSDPlusNevermindJustBSDStill 
nextOS (Just OpenBSDPlusNevermindJustBSDStill) = Just Mac
nextOS (Just Mac) = Just Windows
nextOs (Just Windows) = Nothing

oses :: Maybe OperatingSystem -> [OperatingSystem]
oses p = oses' p [] where 
                oses' Nothing xs = xs
                oses' (Just x) xs = x : oses' (nextOs (Just x)) xs

nextProgLang :: Maybe ProgLang -> Maybe ProgLang
nextProgLang Nothing = Just Haskell
nextProgLang (Just Haskell) = Just Agda
nextProgLang (Just Agda) = Just Idris
nextProgLang (Just Idris) = Just PureScript
nextProgLang (Just PureScript) = Nothing

proglangs :: Maybe ProgLang -> [ProgLang]
proglangs p = proglangs' p [] where 
                proglangs' Nothing xs = xs
                proglangs' (Just x) xs = x : proglangs' (nextProgLang (Just x)) xs

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = o, lang = l} | l <- (proglangs (Just Haskell)), o <- (oses (Just GnuPlusLinux))]

data BinaryTree a =Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
            | b == a = Node left a right
            | b < a= Node (insert' b left) a right
            | b > a= Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf)
            1
            (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf)
              2
              (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay =
        if mapTree (+1) testTree' == mapExpected
        then print "yup OK!"
        else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder t =  preorder' t [] where
              preorder' Leaf _ = []
              preorder' (Node left x right)  xs = [x] ++ (preorder' left xs)  ++ (preorder' right xs)

inorder :: BinaryTree a -> [a]
inorder t =  inorder' t [] where
              inorder' Leaf _ = []
              inorder' (Node left x right)  xs = (inorder' left xs) ++ [x] ++ (inorder' right xs)

postorder :: BinaryTree a -> [a]
postorder t = postorder' t [] where
              postorder' Leaf _ = []
              postorder' (Node left x right)  xs = (postorder' left xs) ++ (postorder' right xs) ++ [x]

breadthFirst :: BinaryTree a -> [a]
breadthFirst t = let (xs, _) = breadthFirst' t ([], [])
                 in xs

breadthFirst' :: BinaryTree a -> ([a], [a]) -> ([a], [a]) 
breadthFirst' Leaf _ = ([], []) 
breadthFirst' (Node l a r) (ns, []) = breadthFirst' (Node l a r) (ns, [a]) 
breadthFirst' (Node l a r) (ns, (q:qs)) =  let (lns, lqs) = breadthFirst' l ([], [])
                                               (rns, rqs) = breadthFirst' r ([], [])
                                           in (q : (lns ++ rns), qs ++ lqs ++ rqs)

testTree :: BinaryTree Integer
testTree =
        Node (Node Leaf 1 Leaf)
        2
        (Node Leaf 3 Leaf)
testPreorder :: IO ()
testPreorder =
        if preorder testTree == [2, 1, 3]
        then putStrLn "Preorder fine!"
        else putStrLn "Bad news bears."
testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."
testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears"

runTreeTests :: IO ()
runTreeTests = do
  testPreorder
  testInorder
  testPostorder


foldTree :: (a->b->b) -> b -> BinaryTree a -> b
foldTree f b Leaf = b
foldTree f b (Node l a r) = f a (foldTree f (foldTree f b r) l)
