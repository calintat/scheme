module Main where

import Control.Applicative ((<$>))
import Control.Monad.Except
import System.Console.Haskeline
import System.Environment
import System.IO

import Core.Error
import Core.Language
import Core.Primitives

import Repl.Eval
import Repl.Parser

import Utils.IO
import Utils.Storage

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runFile args

-- runs the read-eval-print loop
runRepl :: IO ()
runRepl = do
  env <- newEnv
  runIOThrows $ loadStdLib env
  runInputT defaultSettings $ replBody env

-- creates new environment with the primitive bindings
newEnv :: IO SchemeEnv
newEnv = nullEnv >>= flip bindVars (primitives ++ ioPrimitives)

-- runs the action and catches the errors
runIOThrows :: IOThrowsError String -> IO String
runIOThrows = (extractValue <$>) . runExceptT . trapError

-- loads the standard library
loadStdLib :: SchemeEnv -> IOThrowsError String
loadStdLib = flip loadFile "lib/stdlib.scm"

-- loads a program from a file and returns the result
loadFile :: SchemeEnv -> String -> IOThrowsError String
loadFile env file = show <$> eval env (List [Atom "load", String file])

-- runs the body of the repl
replBody :: SchemeEnv -> InputT IO ()
replBody env = do
  input <- getInputLine "scheme>>> "
  case input of
    Nothing -> return ()
    Just expr -> unless (expr == ":quit") $ evalPrint env expr >> replBody env

-- evaluates a string and prints the result
evalPrint :: SchemeEnv -> String -> InputT IO ()
evalPrint env = (>>= outputStrLn) . liftIO . evalString env

-- parses and evaluates a string and traps the errors
evalString :: SchemeEnv -> String -> IO String
evalString env expr = runIOThrows $ show <$> evaluated
  where evaluated = liftThrows (readExpr expr) >>= eval env

-- runs a file as a program with additional arguments
runFile :: [String] -> IO ()
runFile xs = do
  let { file = head xs; args = tail xs }
  env <- newEnvWith ("args", List $ map String args)
  hPutStrLn stderr =<< runIOThrows (loadStdLib env >> loadFile env file)

-- creates new environment with the given binding
newEnvWith :: (String, SchemeVal) -> IO SchemeEnv
newEnvWith = (newEnv >>=) . flip bindVars . return