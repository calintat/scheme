{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad.Except
import Data.ByteString.Char8 (unpack)
import Development.IncludeFile
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

$(includeFileInSource "lib/stdlib.scm" "stdlib")

version = "Scheme interpreter, version 0.1.0.3"

help = "To report an issue, go to https://github.com/calintat/scheme"

main :: IO ()
main = do
  args <- getArgs
  case args of
      [] -> runRepl
      ["--version"] -> putStrLn version
      ["--help"] -> putStrLn help
      _ -> runFile args

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
loadStdLib env = const "stdlib loaded" <$> readStdLib env

-- reads the standard library from the source
readStdLib :: SchemeEnv -> IOThrowsError [SchemeVal]
readStdLib env = liftIO (evaluate $ unpack stdlib) >>= readAndEval env

-- reads and evaluates expressions from a string
readAndEval :: SchemeEnv -> String -> IOThrowsError [SchemeVal]
readAndEval env str = liftThrows (readExprList str) >>= mapM (eval env)

-- loads a program from a file and returns the result
loadFile :: SchemeEnv -> String -> IOThrowsError String
loadFile env file = show <$> eval env (List [Atom "load", String file])

-- runs the body of the repl
replBody :: SchemeEnv -> InputT IO ()
replBody env = do
  input <- getInputLine "scheme>>> "
  case input of
    Nothing -> return ()
    Just "" -> replBody env
    Just ":quit" -> return ()
    Just expr -> evalPrint env expr >> replBody env

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
