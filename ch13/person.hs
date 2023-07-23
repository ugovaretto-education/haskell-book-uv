module Main where
import System.IO (BufferMode(NoBuffering),hSetBuffering, stdout) 
import Text.Read--

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)
mkPerson :: Name
  -> Age
  -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++
    " Age was: " ++ show age

gimmePerson :: String -> Integer -> IO ()
gimmePerson name age = do
  case (mkPerson name age) of
    Right p@(Person n age) -> putStrLn $ "Yay! Successfully got a person: " ++ (show p)
    Left NameEmpty -> putStrLn "Invalid person: name empty"
    Left AgeTooLow -> putStrLn "Invalid person: age too low"
    Left (PersonInvalidUnknown xs) -> putStrLn $ "Invalid person: " ++ xs


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Name Age: "
  line <- getLine
  let ws = words line
  case ws of
    [name, age] -> case (readMaybe age :: Maybe Integer) of
                      Just a -> gimmePerson name a
                      _ -> putStrLn "Input error: invalid age"
    _ -> putStrLn "Input error: Enter name and age"
