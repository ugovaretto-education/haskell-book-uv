module Main where
import Control.Monad
import Data.Char
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let ls = fmap toLower $ filter isAlpha line1
  case (ls == reverse ls) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"

main :: IO ()
main = do
  palindrome
