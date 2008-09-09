module Primitives where

import Control.Monad
import Data.Array
import Defs
import Error

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
              ("=", numBoolMultiOp (==)),
              ("<", numBoolMultiOp (<)),
              (">", numBoolMultiOp (>)),
--              ("/=", numBoolBinOp (/=)),
              (">=", numBoolMultiOp (>=)),
              ("<=", numBoolMultiOp (<=)),
--               ("&&", boolBoolBinOp (&&)),
--               ("||", boolBoolBinOp (||)),
              ("and", boolBoolMultiOp (&&)),
              ("or", boolBoolMultiOp (||)),
              -- need case insensitive versions 
              ("string=?", strBoolBinOp (==)),
              ("string<?", strBoolBinOp (<)),
              ("string>?", strBoolBinOp (>)),
              ("string<=?", strBoolBinOp (<=)),
              ("string=>?", strBoolBinOp (>=)),
              ("make-string", make_string),
              ("string", create_string),
              ("string-length", string_length),
              ("string-ref", string_ref),
              ("string-append", string_append),
              ("substring", subString),
              ("string->list", string2list),
              ("list->string", list2string),
{--
              ("string-set!", -- not yet
              ("string-fill!", -- not yet
              ("string-copy", -- not yet
--}
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
              ("string->symbol", stringToSymbol),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("equal?", equal)
             ]

string2list :: [LispVal] -> ThrowsError LispVal
string2list [(String s)] = return $ List (map (\c -> Character c) s)
string2list [badArg] = throwError $ TypeMismatch "string" badArg
string2list badArgList = throwError $ NumArgs 1 badArgList

list2string :: [LispVal] -> ThrowsError LispVal
list2string [arg@(List s)] = do case foldM toCharacter [] (reverse s) of
                                  Just ss -> return $ String $ reverse ss
                                  Nothing -> throwError $ TypeMismatch "list of characters" arg
    where toCharacter :: [Char] -> LispVal -> Maybe [Char]
          toCharacter acc (Character c) = Just (c:acc)
          toCharacter _ _ = Nothing
list2string [badArg] = throwError $ TypeMismatch "list" badArg
list2string badArgList = throwError $ NumArgs 1 badArgList

make_string :: [LispVal] -> ThrowsError LispVal
make_string [(Number n)] = return $ String (replicate (fromInteger n) ' ')
make_string [(Number n),(Character c)] = return $ String (replicate (fromInteger n) c)
make_string [badArg] = throwError $ TypeMismatch "number" badArg
make_string badArgList = throwError $ NumArgs 2 badArgList

create_string :: [LispVal] -> ThrowsError LispVal
create_string [] = return $ String ""
create_string ((Character c):cs) = do (String tailString) <- create_string cs
                                      return $ String (c:tailString)
create_string (badArg:_) = throwError $ TypeMismatch "character" badArg

string_length :: [LispVal] -> ThrowsError LispVal
string_length [(String s)] = return $ Number (toInteger $ length s)
string_length [badArg] = throwError $ TypeMismatch "string" badArg
string_length badArgList = throwError $ NumArgs 1 badArgList

string_append :: [LispVal] -> ThrowsError LispVal
string_append [(String s0), (String s1)] = return $ String (s0 ++ s1)
string_append badArgs@[_, _] = throwError $ TypeMismatch "string" (List badArgs)
string_append badArgList = throwError $ NumArgs 2 badArgList

subString  :: [LispVal] -> ThrowsError LispVal
subString [(String s), (Number start), (Number end)]
    | (start > end) = throwError $ Default "end must be greater than or equal to start"
    | (fromInteger start >= length s) = throwError $ OutOfRange (Number start)
    | (fromInteger end >= length s) = throwError $ OutOfRange (Number end)
    | otherwise = return $ String (drop (fromInteger start) $ take (fromInteger end) s)
subString badArgs@[_, _, _] = throwError $ TypeMismatch "string, number and number" (List badArgs)
subString badArgList = throwError $ NumArgs 3 badArgList

string_ref :: [LispVal] -> ThrowsError LispVal
string_ref [(String s), (Number n)]
    = if (fromInteger n >= length s) then throwError $ OutOfRange (Number n)
      else return $ Character (s !! (fromInteger n))
string_ref [badArg, (Number _)] = throwError $ TypeMismatch "string" badArg
string_ref [(String _), badArg] = throwError $ TypeMismatch "number" badArg
string_ref badArgs@[_, _] = throwError $ TypeMismatch "string and number" (List badArgs)
string_ref badArgList = throwError $ NumArgs 2 badArgList


car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [x] t] = return t
cdr [DottedList (_:xs) t] = return $ DottedList xs t
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List ys] = return $ List (x:ys)
cons [x, DottedList ys t] = return $ DottedList (x:ys) t
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

-- hmm, the tutorial's definition of eqv? fails to follow r5rs?
-- need to revisit when variable binding is introduced.
eqv' :: [LispVal] -> Bool
eqv' [Bool x, Bool y] = x == y
eqv' [Atom s1, Atom s2] = s1 == s2
eqv' [Number n1, Number n2] = n1 == n2
eqv' [Character c1, Character c2] = c1 == c2
eqv' [List [], List []] = True
eqv' [_, _] = False

eqv :: [LispVal] -> ThrowsError LispVal
eqv [x, y] = return $ Bool (eqv' [x,y])
-- there should be cases for pairs, vectors, functions and so on, but that is saved until variable binding is introduced
eqv badArgList = throwError $ NumArgs 2 badArgList

-- again, equal? from the tutorial coerces non-numeric types such as
-- string into number, but I cannot such a feature in r5rs. Maybe I am
-- wrong.
equal :: [LispVal] -> ThrowsError LispVal
equal [List badArgList1@(x:xs), List badArgList2@(y:ys)]
    = do b1 <- equal [x,y]
         b2 <- equal [List xs, List ys]
         case (b1, b2) of
                    (Bool True, Bool True) -> return $ Bool True
                    (Bool _, Bool _) -> return $ Bool False
                    (_, _) -> throwError $ TypeMismatch "same type" $ List $ badArgList1++badArgList2
equal [Vector v1, Vector v2] 
    = do bs <- mapM (\(e1, e2) -> equal [e1, e2]) $ zip (elems v1) (elems v2)
         boolBoolMultiOp (&&) bs

equal [x, y] = eqv [x,y]
equal badArgList = throwError $ NumArgs 2 badArgList

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


-- I need to learn existential type to do this kind of things:
-- boolMultiOp :: (LispVal -> ThrowsError b) -> (a -> a -> Bool) -> a -> [LispVal] -> ThrowsError LispVal
-- boolMultiOp unpacker op params = do ps <- mapM unpacker params
--                                     return . Bool . foldl (&&) (zipWith op ps (tail ps))

numBoolMultiOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolMultiOp op params = do ps <- mapM unpackNum params
                              return $ Bool  (foldl1 (&&) (zipWith op ps (tail ps)))

boolBoolMultiOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolMultiOp op params = do ps <- mapM unpackBool params
                               return $ Bool  (foldl1 (&&) (zipWith op ps (tail ps)))

-- numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
-- numBoolBinop op singleVal@[_] = throwError $ (NumArgs 2 singleVal)
-- numBoolBinOp op params = mapM unpackNum params >>= return . Bool . foldl1 op

-- boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
-- boolBoolBinop op singleVal@[_] = throwError $ (NumArgs 2 singleVal)
-- boolBoolBinOp op params = mapM unpackBool params >>= return . Bool . foldl1 op

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

boolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp op params = mapM unpackBool params >>= return . Bool . foldl1 op

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp op [p1,p2] = do s1 <- unpackString p1
                             s2 <- unpackString p2
                             return $ Bool (op s1 s2)
strBoolBinOp op params = throwError $ (NumArgs 2 params)

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString notString = throwError $ TypeMismatch "string" notString
