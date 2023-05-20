module Ch8 where
import Data.List (intersperse)

mc91 :: Integer -> Integer
mc91 n 
  | n > 100 = n - 10
  | otherwise = mc91 (n+1)


digitsToWord :: Int -> String
digitsToWord n = concat . intersperse "-" . map wordNumber . digits $ n

digits :: Int -> [Int]
digits x = digits' x []
            where digits' n xs 
                    | n `div` 10 == 0 = n : xs
                    | otherwise = let (n',r) = n `divMod` 10
                                  in digits' n' (r : xs)

wordNumber :: Int -> String
wordNumber 1 = "one"
wordNumber 2 = "two"
wordNumber 3 = "three"
wordNumber _ = "X"

