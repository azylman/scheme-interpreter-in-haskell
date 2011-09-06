-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad

data LispVal =
      Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Char Char

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- TODO: Add support for character names (e.g. #\space) in addition to characters.
parseCharacter :: Parser LispVal
parseCharacter = do
    char '#'
    char '\\'
    rest <- letter <|> symbol
    return $ Char rest

-- TODO: Add support for floats.
-- parseFloat :: Parser Float

-- TODO: Add support for escaped characters (e.g. \", \n, \r, \t, \\).
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        otherwise -> Atom atom

-- TODO: Allow support for different bases.
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr =
        parseCharacter
    <|> parseAtom
    <|> parseString
    <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
