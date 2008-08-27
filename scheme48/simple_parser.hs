module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Numeric
import Data.Array

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char
             | Vector (Array Int LispVal)
--               deriving Show

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom fun : args)) = mapM eval args >>= apply fun
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply fun args = maybe (throwError $ NotFunction "Unrecognized primitive function args" fun)
                 ($ args)
                 (lookup fun primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
              ("+", numericBinOp (+)),
              ("-", numericBinOp (-)),              
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),              
              ("mod", numericBinOp mod),              
              ("quotient", numericBinOp quot),             
              ("remainder", numericBinOp rem),
              ("string?", isString),
              ("symbol?", isSymbol),
              ("number?", isNumber),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol)
             ]

isString :: [LispVal] -> ThrowsError LispVal
isString ((String _):_) = return $ Bool True
isString _ = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber ((Number _):_) = return $ Bool True
isNumber _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol ((Atom _):_) = return $ Bool True
isSymbol _ = return $ Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString ((Atom s):_) = return $ String s
symbolToString notSymbol = throwError $ TypeMismatch "symbol" $ notSymbol !! 0

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol ((String s):_) = return $ Atom s
stringToSymbol notString = throwError $ TypeMismatch "string" $ notString !! 0

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op singleVal@[_] = throwError $ (NumArgs 2 singleVal)
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                        if null parsed
                           then throwError $ TypeMismatch "number" $ String n
                           else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal contents = show contents

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

parseVector :: Parser LispVal
parseVector = do
  string "#("
  List l <- parseList
  char ')'
  return $ Vector (array (1, length l) $ zip [1 .. length l] l)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
  char '`'
  quasiquoted <- parseExpr
  return $ List [Atom "quasiquote", quasiquoted]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  char ','
  unq <- parseExpr
  return $ List [Atom "unquote", unq]

parseUnquotedSplicing :: Parser LispVal
parseUnquotedSplicing = do
  string ",@"
  unquotedSplicing <- parseExpr
  return $ List [Atom "unquote-splicing", unquotedSplicing]
  
parseCharacter :: Parser LispVal
parseCharacter = do string "#\\"
                    c <- anyChar
                    return $ Character c

parseString' :: Parser LispVal
parseString' = do char '"'
                  x <- many $ noneOf "\""
                  char '"'
                  return $ String x

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ parseStringContent
                 char '"'
                 return $ String x
    where parseStringContent = escapedChar <|> noneOf "\""
          escapedChar = do char '\\'
                           e <- oneOf "\"nrt\\"
                           return $ case e of
                                      'n' -> '\n'
                                      'r' -> '\r'
                                      '\\' -> '\\'
                                      '"' -> '"'


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = (first:rest)
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber :: Parser LispVal
parseNumber = try parseFloat <|> try parseRadixedNumber <|> parseDecNumber

parseFloat :: Parser LispVal
parseFloat = do s <- option '+' (oneOf "+-")
                x <- option "0" (many1 digit)
                char '.'
                y <- many1 digit
                return $ case s of
                           '+' -> Float (read (x ++ "." ++ y))
                           '-' -> Float (-1.0 * read (x ++ "." ++ y))

parseDecNumber :: Parser LispVal
parseDecNumber = liftM (Number . read) $ many1 digit

parseRadixedNumber :: Parser LispVal
parseRadixedNumber = do char '#'
                        radixChar <- oneOf "bodx"
                        case radixChar of
                          'b' -> do binary <- many1 (oneOf "01")
                                    return $ (Number . readBin) binary
                          'o' -> do octal <- many1 (oneOf "01234567")
                                    return $ (Number . readOct') octal
                          'd' -> parseDecNumber
                          'x' -> do hexadecimal <- many1 (digit <|> oneOf "abcdefABCDEF")
                                    return $ (Number . readHex') hexadecimal
    where
      readOct' = fst . head . readOct
      readHex' = fst . head . readHex
      readBin  = foldl bitValue 0
          where bitValue accum binDigit = case binDigit of
                                            '0' -> accum*2
                                            '1' -> accum*2 + 1

parseExpr :: Parser LispVal
parseExpr = try parseCharacter
            <|> try parseNumber
            <|> try parseQuoted
            <|> try parseQuasiquoted
            <|> try parseUnquotedSplicing
            <|> try parseUnquoted
            <|> try parseVector
            <|> parseString
            <|> parseAtom
            <|> do char '('
                   x <- (try parseList) <|> parseDottedList
                   char ')'
                   return x


symbol :: Parser Char
symbol = oneOf "!$#%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                     ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An unknown error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

main :: IO ()
main = do args <- getArgs
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractValue $ trapError evaled
