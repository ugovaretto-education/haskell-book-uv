module Chypher where
import Data.Char

charToCode :: Char -> Int
charToCode c = (ord . toUpper $ c) - ord 'A'

codeToChar :: Int -> Char
codeToChar n = chr (n + ord 'A')

shiftRight :: Int -> Char -> Char
shiftRight s c= let i = charToCode c
                 in chr (((i + s) `mod` 26) + ord 'A')

shiftLeft :: Int -> Char -> Char
shiftLeft s c = let i = charToCode c
                in chr (((i - s + 26) `rem` 26) + ord 'A') 

caesar :: String -> Int -> String
caesar [] _ = []
caesar xs n = map (\x -> if x == ' ' then ' ' else (shiftRight n x)) xs

unCaesar :: String -> Int -> String
unCaesar [] _ = []
unCaesar xs n = map (\x -> if x == ' ' then ' ' else (shiftLeft n x)) xs
------------------------------------------------------------------------
testText = "MEET AT DAWN"
testTkey = "ALLY"

vignere :: String -> String -> String
vignere ws' ks' = vignere' ws' ks' [] ks' where 
                  vignere' ws [] cs k = vignere' ws k cs k
                  vignere' [] _ cs _ = cs
                  vignere' (w:ws) ts@(k:ks) cs k' 
                                  | w == ' ' = vignere' ws ts (cs ++ [' ']) k'
                                  | otherwise = vignere' ws ks 
                                                   (cs ++ [shiftRight (charToCode k) w]) k'
                                                   
