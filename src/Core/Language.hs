module Core.Language where

import Control.Monad.Except
import Data.Array
import Data.Complex
import Data.IORef
import Data.Ratio
import System.IO
import Text.Printf (printf)

import Core.Error

-- SCHEME DATA TYPES -----------------------------------------------------------

type SchemeEnv = IORef [(String, IORef SchemeVal)]

type SchemeFun = [SchemeVal] -> ThrowsError SchemeVal

data SchemeVal = Atom String

               | Boolean Bool

               | Char Char

               | String String

               | Integer Int

               | Rational Rational

               | Real Double

               | Complex (Complex Double)

               | List [SchemeVal]

               | ImproperList [SchemeVal] SchemeVal

               | Vector (Array Int SchemeVal)

               | Bytevector (Array Int Integer)

               | Primitive SchemeFun

               | Func { args :: [String],

                        vararg :: Maybe String,

                        body :: [SchemeVal],

                        closure :: SchemeEnv }

               | IOPrimitive ([SchemeVal] -> IOThrowsError SchemeVal)

               | IOPort Handle

               | Unspecified

-- STRING REPRESENTATIONS ------------------------------------------------------

instance Show SchemeVal where show = showVal

showVal (Atom name) = name

showVal (Boolean bool) = if bool then "#t" else "#f"

showVal (Char ch) = show ch

showVal (String str) = printf "\"%s\"" str

showVal (Integer n) = show n

showVal (Rational q) = printf "%d/%d" (numerator q) (denominator q)

showVal (Real x) = show x

showVal (Complex z) = printf "%f+%fi" (realPart z) (imagPart z)

showVal (List xs) = printf "(%s)" (unwordsList xs)

showVal (ImproperList xs x) = printf "(%s . %s)" (unwordsList xs) (showVal x)

showVal (Vector array) = printf "#(%s)" (unwordsArray array)

showVal (Bytevector array) = printf "#u8(%s)" (unwordsArray array)

showVal (Primitive _) = "<primitive>"

showVal (Func args vararg _ _) = showLambda args vararg

showVal (IOPrimitive _) = "<IO primitive>"

showVal (IOPort _) = "<IO port>"

showVal Unspecified = "<unspecified>"

-- UTILS -----------------------------------------------------------------------

-- throws a wrong number of arguments error
throwWrongNumArgs :: Int -> [SchemeVal] -> ThrowsError SchemeVal
throwWrongNumArgs = (throwError .) . (. unwordsList) . WrongNumArgs

-- throws a type mismatch error
throwTypeMismatch :: String -> SchemeVal -> ThrowsError SchemeVal
throwTypeMismatch = (throwError .) . (. show) . TypeMismatch

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

unwordsArray :: Show a => Array Int a -> String
unwordsArray = unwordsList . elems

showLambda :: [String] -> Maybe String -> String
showLambda = (. maybe [] (" . " ++)) . printf "(lambda (%s%s) ...)" . unwords
