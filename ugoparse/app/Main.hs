module Main where

import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number
import System.Environment as E
import System.Exit

-- Parser
type Var = String

data Statement
  = Print Var
  | Assign Var Int
  | Add Var Var
  | While Var Code
  deriving (Eq, Show)

type Code = [Statement]

ms :: Parser String
ms = many1 space

ml :: Parser String
ml = many1 letter

statementParser :: Parser Statement
statementParser =
  try printParser <|> try assignParser <|> try addParser <|> whileParser

printParser :: Parser Statement
printParser = Print <$> (string "print" >> many1 space >> many1 letter)

assignParser :: Parser Statement
assignParser =
  Assign <$> (many1 letter) <*> (many1 space >> char '=' >> many1 space >> int)

addParser :: Parser Statement
addParser =
  Add <$> (many1 letter) <*>
  (many1 space >> string "+=" >> many1 space >> many letter)

whileParser :: Parser Statement
whileParser =
  While <$> (string "while" >> ms >> ml) <*>
  (ms >> string "positive" >> ms >> char '{' >> ms >>
   many (statementParser <* ms) <* char '}')

codeParser :: Parser Code
codeParser = spaces >> many (statementParser <* spaces) <* eof

-- C generator
statementToC :: Statement -> String
statementToC (Assign v n) = v ++ " = " ++ (show n) ++ ";"
statementToC (Add a b) = a ++ " += " ++ b ++ ";"
statementToC (Print v) = "printf(\"%d\"," ++ v ++ ");"
statementToC (While v c) =
  "while (" ++ v ++ "> 0) {\n" ++ statementsToC c ++ "}"

statementsToC :: [Statement] -> String
statementsToC stmts = unlines (map statementToC stmts)

statementVariable :: Statement -> [Var]
statementVariable (Print v) = [v]
statementVariable (Assign v n) = [v]
statementVariable (Add a b) = [a, b]
statementVariable (While v c) = [v] ++ statementsVariables c

statementsVariables :: [Statement] -> [Var]
statementsVariables = concat . map statementVariable

codeVariables :: [Statement] -> [Var]
codeVariables = S.toList . S.fromList . statementsVariables

makeDeclaration :: Var -> String
makeDeclaration v = "int " ++ v ++ ";"

makeDeclarations :: [Var] -> String
makeDeclarations = unlines . map makeDeclaration

codeToC :: Code -> String
codeToC code =
  "#include <stdio.h>\nint main() {\n" ++
  makeDeclarations (codeVariables code) ++ statementsToC code ++ "return 0;\n}\n"

-- Main
main :: IO ()
main = do
  args <-E.getArgs
  case args of
    [inputFile, outputFile] -> do
                fileText <- readFile inputFile
                let parsed = parse codeParser "" fileText
                case parsed of
                  Left e -> print e
                  Right c -> writeFile outputFile (codeToC c)
    _ -> putStrLn "No input and output file names specified"

