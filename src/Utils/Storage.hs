module Utils.Storage where

import Control.Monad.Except
import Data.IORef
import Data.Maybe

import Core.Error
import Core.Language

type Binding = (String, SchemeVal)

-- creates an empty environment
nullEnv :: IO SchemeEnv
nullEnv = newIORef []

-- determine if a given variable is already bound in the environment
isBound :: SchemeEnv -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

-- retrieve the current val of a variable
getVar :: SchemeEnv -> String -> IOThrowsError SchemeVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar var) (liftIO . readIORef) (lookup var env)

-- sets a variable
setVar :: SchemeEnv -> String -> SchemeVal -> IOThrowsError SchemeVal
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar var) (liftIO . flip writeIORef val) (lookup var env)
  return val

-- sets a variable if already bound or creates a new one if not
defineVar :: SchemeEnv -> String -> SchemeVal -> IOThrowsError SchemeVal
defineVar envRef var val = do
  defined <- liftIO $ isBound envRef var
  if defined then setVar envRef var val >> return val
             else liftIO $ createVar envRef var val

-- creates a new variable
createVar :: SchemeEnv -> String -> SchemeVal -> IO SchemeVal
createVar envRef var val = do
  valRef <- newIORef val
  env <- readIORef envRef
  writeIORef envRef $ (var, valRef) : env
  return val

-- bind multiple variables at once
bindVars :: SchemeEnv -> [(String, SchemeVal)] -> IO SchemeEnv
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = (++ env) <$> mapM add bindings
        add (var, val) = (,) var <$> newIORef val
