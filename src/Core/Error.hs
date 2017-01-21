module Core.Error where

import Control.Monad.Except
import Text.Printf (printf)

-- ERROR DATA TYPE -------------------------------------------------------------

data SchemeErr = Default String

               | Parsing String

               | UnboundFun String

               | UnboundVar String

               | WrongNumArgs Int String

               | TypeMismatch String String

-- ERROR UTILS -----------------------------------------------------------------

type ThrowsError = Either SchemeErr

type IOThrowsError = ExceptT SchemeErr IO

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = either throwError return

trapError action = catchError action $ return . show

-- ERROR MESSAGES --------------------------------------------------------------

instance Show SchemeErr where show = showErr

showErr :: SchemeErr -> String

showErr (Default message) = showDefault message

showErr (Parsing message) = showParsing message

showErr (UnboundFun name) = showUnboundFun name

showErr (UnboundVar name) = showUnboundVar name

showErr (WrongNumArgs expected found) = showWrongNumArgs expected found

showErr (TypeMismatch expected found) = showTypeMismatch expected found

-- returns a default error message
showDefault :: String -> String
showDefault = printf "Default error: %s"

-- returns a parsing error message
showParsing :: String -> String
showParsing = printf "Parsing error: %s"

-- returns an unbound function error message
showUnboundFun :: String -> String
showUnboundFun = printf "Unbound function: %s"

-- returns an unbound variable error message
showUnboundVar :: String -> String
showUnboundVar = printf "Unbound variable: %s"

-- returns a wrong number of arguments error message
showWrongNumArgs :: Int -> String -> String
showWrongNumArgs = printf "Expected %d args; found %s"

-- returns a type mismatch error message
showTypeMismatch :: String -> String -> String
showTypeMismatch = printf "Expected type %s; found %s"
