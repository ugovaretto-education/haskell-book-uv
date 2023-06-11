{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Set as S
import Data.Text as T (Text)
import Data.Void
import System.Environment as E
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- type Parsec e s a = ParsecT e s Identity a
type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

symbol :: String -> Parser String
symbol = L.symbol sc

-- Parser
type Var = String

data Statement
  = Print Var
  | Assign Var Integer
  | Add Var Var
  | While Var Code
  deriving (Eq, Show)

type Code = [Statement]

statementParser :: Parser Statement
statementParser =
  try printParser <|> try assignParser <|> try addParser <|> whileParser

printParser :: Parser Statement
printParser =
  Print <$> (string "print" >> sc >> (many alphaNumChar <* sc))

assignParser :: Parser Statement
assignParser =
  Assign <$> (many alphaNumChar) <*> (sc >> char '=' >> sc >> L.signed sc L.decimal)

addParser :: Parser Statement
addParser =
  Add <$> (many alphaNumChar) <*>
  (sc >> string "+=" >> sc >> many alphaNumChar)

whileParser :: Parser Statement
whileParser =
  While <$> (string "while" >> sc >> ml) <*>
  (sc >> string "positive" >> sc >> char '{' >> sc >>
   many (statementParser <* sc) <* char '}')

ml :: Parser String
ml = many alphaNumChar

codeParser :: Parser Code
codeParser = sc >> many (statementParser <* sc) <* eof

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
  makeDeclarations (codeVariables code) ++
  statementsToC code ++ "return 0;\n}\n"

-- Main
main :: IO ()
main = do
  args <- E.getArgs
  case args of
    [inputFile, outputFile] -> do
      fileText <- readFile inputFile
      let parsed = parse codeParser "" fileText
      case parsed of
        Left e -> putStrLn $ errorBundlePretty e
        Right c -> writeFile outputFile (codeToC c)
    _ -> putStrLn "No input and output file names specified"
