module Error(throwError, LispError(..), ThrowsError, extractValue, trapError) where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error
import Defs
import Text.Show

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | OutOfRange LispVal
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
showError (OutOfRange found) = "argument out of range: " ++ show found
showError (Default message) = "error: " ++ message

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An unknown error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
