module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Complex
import Data.Ratio

main :: IO ()
main = do
    args <- getArgs
    putStrLn (args !! 0)
    putStrLn (readExpr (args !! 0)) 

data LispVal = Atom String
    | String String
    | Bool Bool
    | List [LispVal]
    | ImproperList [LispVal] LispVal
    | Number Integer
    | Float Double
    | Character Char
    | Rational (Ratio Integer)
    | Complex (Complex Double)
    deriving Show

parseSymbol :: Parser Char
parseSymbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseSpaces :: Parser ()
parseSpaces = skipMany1 space

parseEscape :: Parser Char
parseEscape = do char '\\'
                 x <- anyChar
                 return $ case x of
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'
                    _    -> x

parseString :: Parser LispVal
parseString = do char '\"'
                 x <- many (noneOf "\\\"" <|> parseEscape)
                 char '\"'
                 return $ String x

parseBool :: Parser LispVal
parseBool = do char '#'
               x <- oneOf "tf"
               return $ case x of
                't' -> Bool True
                'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> parseSymbol
               rest <- many (letter <|> digit <|> parseSymbol)
               return $ String (first : rest)

parseImplicitDec :: Parser LispVal
parseImplicitDec = do num <- many1 digit
                      return $ (Number . read) num
--equivalent definitions:
--parseImplicitDec = liftM (Number . read) $ many1 digit
--parseImplicitDec = many1 digit >>= (\num -> return $ (Number . read) num)

parseDec :: Parser LispVal
parseDec = do string "#d"
              num <- many1 digit
              return $ (Number . read) num

convertHex :: String -> Integer
convertHex x = fst( readHex x !! 0)

parseHex :: Parser LispVal
parseHex = do string "#h"
              num <- many1 hexDigit
              return $ (Number . convertHex) num

convertOct :: String -> Integer
convertOct x = fst( readOct x !! 0)

parseOct :: Parser LispVal
parseOct = do string "#o"
              num <- many1 octDigit
              return $ (Number . convertOct) num

convertBin :: String -> Integer
convertBin [] = 0
convertBin x = (read [last x]) + 2 * (convertBin $ init x)

parseBin :: Parser LispVal
parseBin = do string "#b"
              num <- many1 (oneOf "01")
              return $ (Number . convertBin) num

parseNumber :: Parser LispVal
parseNumber = try parseImplicitDec
                <|> try parseDec
                <|> try parseHex
                <|> try parseOct
                <|> try parseBin

parseCharacter :: Parser LispVal
parseCharacter = do char '#'
                    x <- try(string "\\newline"
                         <|> string "\\space")
                         <|> do{y <- parseEscape; return [y]}
                    notFollowedBy alphaNum
                    return $ Character (case x of
                        "space"   -> ' '
                        "newline" -> '\n'
                        _         -> x !! 0)

parseFloat :: Parser LispVal
parseFloat = do whole <- many digit
                char '.'
                decimal <- many digit
                let num = (whole ++ ['.'] ++ decimal) in
                    return $ Float $ fst (readFloat num !! 0)

parseRational :: Parser LispVal
parseRational = do num <- many1 digit
                   char '/'
                   denom <- many1 digit
                   return $ Rational $ (read num) % (read denom)

getFloatVal :: LispVal -> Double
getFloatVal (Float x) = x
getFloatVal (Number x) = fromIntegral x

parseComplex :: Parser LispVal
parseComplex = do real <- (try parseFloat <|> parseNumber)
                  char '+'
                  imag <- (try parseFloat <|> parseNumber)
                  char 'i'
                  return $ Complex ((getFloatVal real) :+ (getFloatVal imag))

parseExpr :: Parser LispVal
parseExpr = try parseString
            <|> try parseAtom
            <|> try parseBool
            <|> try parseCharacter
            <|> try parseComplex
            <|> try parseFloat
            <|> try parseRational
            <|> try parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
