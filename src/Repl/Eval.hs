module Repl.Eval where

import Control.Monad.Except
import Data.Maybe
import Text.ParserCombinators.Parsec

import Core.Error
import Core.Language

import Repl.Parser

import Utils.Storage

eval :: SchemeEnv -> SchemeVal -> IOThrowsError SchemeVal
eval env (Atom name) = getVar env name
eval env val@(Boolean _) = return val
eval env val@(Char _) = return val
eval env val@(String _) = return val
eval env val@(Integer _) = return val
eval env val@(Rational _) = return val
eval env val@(Real _) = return val
eval env val@(Complex _) = return val
eval env val@(Vector _) = return val
eval env val@(Bytevector _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "unquote", val]) = eval env val
eval env (List [Atom "quasiquote", val]) = return val
eval env (List [Atom "if", cond, conseq]) = eval env (List [Atom "if", cond, conseq, Unspecified])
eval env (List [Atom "if", cond, conseq, alt]) = do
  result <- eval env cond
  eval env $ if evalAsBool result then conseq else alt
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : ImproperList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : ImproperList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
    load filename >>= fmap last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ Default $ show badForm

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarargs = makeFunc . Just . showVal

evalAsBool :: SchemeVal -> Bool
evalAsBool (Boolean False) = False
evalAsBool _ = True

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser = either (throwError . Parsing . show) return . parse parser "scheme"

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

load :: String -> IOThrowsError [SchemeVal]
load = (liftThrows . readExprList =<<) . liftIO . readFile

apply :: SchemeVal -> [SchemeVal] -> IOThrowsError SchemeVal
apply (Primitive func) args = liftThrows $ func args
apply (IOPrimitive func) args = func args
apply (Func params varargs body closure) args =
      if length params /= length args && isNothing varargs
         then throwError $ WrongNumArgs (length params) (unwords $ map show args)
         else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            evalBody env = last <$> mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                Nothing -> return env
apply fun _ = throwError $ UnboundFun $ show fun
