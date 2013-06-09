module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0)) 

data LispVal = Atom String
    | String String
    | Bool Bool
    | List [LispVal]
    | ImproperList [LispVal] LispVal
    | Number Integer

parseSymbol :: Parser Char
parseSymbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseSpaces :: Parser ()
parseSpaces = skipMany1 space

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> parseSymbol
               rest <- many (letter <|> digit <|> parseSymbol)
               let atom = first : rest
               return $ case atom of
                    "#t" -> Bool True
                    "#f" -> Bool False
                    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseNumber <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "scheme" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


