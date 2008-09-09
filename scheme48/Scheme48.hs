module Main where

import System.Environment
import Control.Monad
import Data.List
import Defs
import Error
import Parser
import Show
import Primitives

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval val@(Vector _) = return val
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         otherwise -> eval conseq
eval (List (Atom "begin":cs)) = begin $ List cs
eval (List (Atom "cond":cs)) = evalCond cs
    where
      evalCondPred p
          = case p of
              Atom "else" -> return $ Bool True
              otherwise -> eval p
      evalCond ((List (pred:body)):cs)
          = do result <- evalCondPred pred
               case result of
                 Bool True -> begin $ List body
                 otherwise -> evalCond cs
      evalCond _ = throwError $ BadSpecialForm "no matching clause for cond" (List cs)
eval (List (Atom "case":key:clauses))
     = do evaled_key <- eval key
          let clause = find (matchCaseHead evaled_key) clauses
          case clause of
            Nothing -> throwError $ BadSpecialForm "no matching clause for case" (List clauses)
            Just (List (head:body)) -> begin $ List body
              
eval (List [Atom "quote", val]) = return val
eval (List (Atom fun : args)) = mapM eval args >>= apply fun
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

matchCaseHead key (List ((List head):_)) = any (\d -> eqv' [key, d]) head
matchCaseHead _ (List (Atom "else":_)) = True

begin (List [e]) = eval e
begin (List (e:es)) = eval e >> (begin $ List es)

apply :: String -> [LispVal] -> ThrowsError LispVal
apply fun args = maybe (throwError $ NotFunction "Unrecognized primitive function args" fun)
                 ($ args)
                 (lookup fun primitives)

main :: IO ()
main = do args <- getArgs
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractValue $ trapError evaled


