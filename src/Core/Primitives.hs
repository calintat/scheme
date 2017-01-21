module Core.Primitives where

import Control.Arrow
import Control.Monad.Except
import Data.Array

import Core.Error
import Core.Language

primitives :: [(String, SchemeVal)]
primitives = (map . second) Primitive primitivesList

primitivesList = [("+", numericBinop (+)),
                  ("-", numericBinop (-)),
                  ("*", numericBinop (*)),
                  ("/", numericBinop div),
                  ("mod", numericBinop mod),
                  ("quotient", numericBinop quot),
                  ("remainder", numericBinop rem),
                  ("char?", unaryOp isChar),
                  ("symbol?", unaryOp isSymbol),
                  ("string?", unaryOp isString),
                  ("number?", unaryOp isNum),
                  ("complex?", unaryOp isNum),
                  ("real?", unaryOp isReal),
                  ("rational?", unaryOp isRational),
                  ("integer?", unaryOp isInteger),
                  ("bool?", unaryOp isBool),
                  ("list?", unaryOp isList),
                  ("symbol->string", unaryOp symbol2string),
                  ("string->symbol", unaryOp string2symbol),
                  ("=", numBoolBinop (==)),
                  ("<", numBoolBinop (<)),
                  (">", numBoolBinop (>)),
                  ("/=", numBoolBinop (/=)),
                  (">=", numBoolBinop (>=)),
                  ("<=", numBoolBinop (<=)),
                  ("&&", boolBoolBinop (&&)),
                  ("||", boolBoolBinop (||)),
                  ("string=?", strBoolBinop (==)),
                  ("string<?", strBoolBinop (<)),
                  ("string>?", strBoolBinop (>)),
                  ("string<=?", strBoolBinop (<=)),
                  ("string>=?", strBoolBinop (>=)),
                  ("string->number", unaryOp string2number),
                  ("car", car),
                  ("cdr", cdr),
                  ("cons", cons),
                  ("eq?", eqv),
                  ("eqv?", eqv),
                  ("equal?", eqv),
                  ("make-string", makeString),
                  ("string", string),
                  ("string-length", stringLength),
                  ("string-ref", stringRef)]

numericBinop :: (Int -> Int -> Int) -> [SchemeVal] -> ThrowsError SchemeVal
numericBinop op []      = throwError $ WrongNumArgs 2 []
numericBinop op val@[_] = throwError $ WrongNumArgs 2 $ show val
numericBinop op params  =  Integer . foldl1 op <$> mapM unpackNum params

unpackNum :: SchemeVal -> ThrowsError Int
unpackNum (Integer n) = return n
unpackNum notInteger  = throwError $ TypeMismatch "number" $ show notInteger

unpackStr :: SchemeVal -> ThrowsError String
unpackStr (String s)  = return s
unpackStr (Integer s) = return $ show s
unpackStr (Boolean s) = return $ show s
unpackStr x           = throwError $ TypeMismatch "string" $ show x

unpackBool :: SchemeVal -> ThrowsError Bool
unpackBool (Boolean b) = return b
unpackBool x           = throwError $ TypeMismatch "boolean" $ show x

unaryOp :: (SchemeVal -> SchemeVal) -> [SchemeVal] -> ThrowsError SchemeVal
unaryOp f [v]  = return $ f v
unaryOp f args = throwWrongNumArgs 1 args

boolBinop :: (SchemeVal -> ThrowsError a) -> (a -> a -> Bool) -> [SchemeVal] -> ThrowsError SchemeVal
boolBinop unpacker op args =
  if length args /= 2 then throwWrongNumArgs 2 args else do
    lhs <- unpacker $ head args
    rhs <- unpacker $ last args
    return $ Boolean $ op lhs rhs

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool

isChar (Char _) = Boolean True
isChar _        = Boolean False

isSymbol (Atom _) = Boolean True
isSymbol _        = Boolean False

isString (String _) = Boolean True
isString _          = Boolean False

isBool (Boolean _) = Boolean True
isBool _           = Boolean False

isList (List _)           = Boolean True
isList (ImproperList _ x) = isList x
isList _                  = Boolean False

isPair (List _)           = Boolean True
isPair (ImproperList _ _) = Boolean True
isPair _                  = Boolean False

symbol2string (Atom x) = String x
symbol2string _        = String ""

string2symbol (String x) = Atom x
string2symbol _          = Atom ""

string2number (String x) = Integer $ read x
string2number _          = Unspecified

isNum (Integer n)  = Boolean True
isNum (Rational q) = Boolean True
isNum (Real x)     = Boolean True
isNum (Complex z)  = Boolean True
isNum _            = Boolean False

isInteger (Integer n) = Boolean True
isInteger _           = Boolean False

isRational (Integer n)  = Boolean True
isRational (Rational q) = Boolean True
isRational _            = Boolean False

isReal (Integer n)  = Boolean True
isReal (Rational q) = Boolean True
isReal (Real x)     = Boolean True
isReal _            = Boolean False

isComplex (Integer n)  = Boolean True
isComplex (Rational q) = Boolean True
isComplex (Real x)     = Boolean True
isComplex (Complex z)  = Boolean True
isComplex _            = Boolean False

car :: [SchemeVal] -> ThrowsError SchemeVal
car [List (x : xs)]           = return x
car [ImproperList (x : xs) _] = return x
car [badArg]                  = throwTypeMismatch "pair" badArg
car badArgList                = throwWrongNumArgs 1 badArgList

cdr :: [SchemeVal] -> ThrowsError SchemeVal
cdr [List (x : xs)]           = return $ List xs
cdr [ImproperList [xs] x]     = return x
cdr [ImproperList (_ : xs) x] = return $ ImproperList xs x
cdr [badArg]                  = throwTypeMismatch "pair" badArg
cdr badArgList                = throwWrongNumArgs 1 badArgList

cons :: [SchemeVal] -> ThrowsError SchemeVal
cons [x, List []]           = return $ List [x]
cons [x, List xs]           = return $ List (x:xs)
cons [x, ImproperList xs y] = return $ ImproperList (x:xs) y
cons [x, y]                 = return $ ImproperList [x] y
cons args                   = throwWrongNumArgs 2 args

eqv [Atom arg1, Atom arg2] = return $ Boolean $ arg1 == arg2
eqv [l1@(List arg1), l2@(List arg2)] = eqvList eqv [l1, l2]
eqv [Integer arg1, Integer arg2] = return $ Boolean $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Boolean $ arg1 == arg2
eqv [Boolean arg1, Boolean arg2] = return $ Boolean $ arg1 == arg2
eqv [Char arg1, Char arg2] = return $ Boolean $ arg1 == arg2
eqv [Real arg1, Real arg2] = return $ Boolean $ arg1 == arg2
eqv [Rational arg1, Rational arg2] = return $ Boolean $ arg1 == arg2
eqv [Complex arg1, Complex arg2] = return $ Boolean $ arg1 == arg2
eqv [Vector arg1, Vector arg2] = eqv [List (elems arg1), List (elems arg2)]
eqv [_, _] = return $ Boolean False
eqv args = throwWrongNumArgs 2 args

eqvList :: ([SchemeVal] -> ThrowsError SchemeVal) -> [SchemeVal] -> ThrowsError SchemeVal
eqvList eqvFunc [List arg1, List arg2] = return $ Boolean $ length arg1 == length arg2 && all eqvPair (zip arg1 arg2)
  where eqvPair (x1, x2) = either (const False) fromBoolean $ eqvFunc [x1, x2]
        fromBoolean (Boolean val) = val

makeString [Integer len]          = makeString [Integer len, Char '\0']
makeString [Integer len, Char ch] = return $ String $ replicate len ch
makeString [Integer len, x]       = throwTypeMismatch "char" x
makeString (x:xs)                 = throwTypeMismatch "number" x
makeString args                   = throwWrongNumArgs 2 args

string = stringIter []

stringIter str []               = return $ String str
stringIter str (Char ch : args) = stringIter (str ++ [ch]) args
stringIter str (x:xs)           = throwTypeMismatch "char" x

stringLength [String str] = return . Integer $ length str
stringLength [x]          = throwTypeMismatch "string" x
stringLength args         = throwWrongNumArgs 2 args

stringRef [String str, Integer k] = return $ Char $ str !! k
stringRef [String str, x]         = throwTypeMismatch "number" x
stringRef [x, _]                  = throwTypeMismatch "string" x
stringRef args                    = throwWrongNumArgs 2 args

isEven [Integer n] = return $ Boolean $ even n
isEven [_]         = return $ Boolean False
isEven args        = throwWrongNumArgs 1 args

isOdd [Integer n] = return $ Boolean $ odd n
isOdd [_]         = return $ Boolean False
isOdd args        = throwWrongNumArgs 1 args
