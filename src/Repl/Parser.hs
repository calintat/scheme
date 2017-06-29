module Repl.Parser (parseExpr) where

import Data.Array
import Data.Char
import Data.Complex
import Data.Maybe
import Data.Ratio
import Numeric
import Text.ParserCombinators.Parsec

import Core.Language

parseExpr :: Parser SchemeVal
parseExpr = tryAll parsers

parsers = [parseAtom,
           parseAtomEnclosed,
           parseBoolean,
           parseChar,
           parseHexChar,
           parseSpecialChar,
           parseString,
           parseComplex,
           parseReal,
           parseRational,
           parseInteger,
           parseList,
           parseImproperList,
           parseVector,
           parseBytevector,
           parseQuote,
           parseUnquote,
           parseQuasiquote]

-- PARSERS ---------------------------------------------------------------------

parseAtom :: Parser SchemeVal
parseAtom = do
  str <- many1 $ letter <|> digit <|> symbol
  if isDigit (head str) then fail "digit not allowed" else return $ Atom str

parseAtomEnclosed :: Parser SchemeVal
parseAtomEnclosed = do
  char '|'
  str <- many1 $ letter <|> digit <|> symbol <|> hexChar <|> space
  char '|'
  return $ Atom str

parseBoolean :: Parser SchemeVal
parseBoolean = do
  char '#'
  c <- oneOf "tf"
  return $ Boolean $ c == 't'

parseChar :: Parser SchemeVal
parseChar = do
  string "#\\"
  x <- anyChar
  notFollowedBy alphaNum
  return $ Char x

parseHexChar :: Parser SchemeVal
parseHexChar = fmap (Char . chr . fromHex) (string "#\\x" >> many1 hexDigit)

parseSpecialChar :: Parser SchemeVal
parseSpecialChar = do
  string "#\\"
  str <- tryAll $ map (string . fst) specialChars
  return $ Char $ fromJust $ lookup str specialChars

parseString :: Parser SchemeVal
parseString = do
  char '"'
  str <- many $ tryAll [escChar, hexChar, noneOf "\"\\"]
  char '"'
  return $ String str

parseInteger :: Parser SchemeVal
parseInteger = Integer <$> tryAll [int, hex, dec, oct, bin]

parseRational :: Parser SchemeVal
parseRational = do
  a <- many1 digit
  char '/'
  b <- many1 digit
  return $ Rational $ read a % read b

parseReal :: Parser SchemeVal
parseReal = do
  a <- many1 digit
  char '.'
  b <- many1 digit
  return $ Real $ fst . head . readFloat $ a ++ "." ++ b

parseComplex :: Parser SchemeVal
parseComplex = do
  x <- tryAll [parseReal, parseInteger]
  char '+'
  y <- tryAll [parseReal, parseInteger]
  char 'i'
  return $ Complex $ fromNumber x :+ fromNumber y

parseList :: Parser SchemeVal
parseList = do
  char '('
  xs <- sepBy parseExpr spaces
  char ')'
  return $ List xs

parseImproperList :: Parser SchemeVal
parseImproperList = do
  char '('
  xs <- endBy parseExpr spaces
  char '.'
  x <- spaces >> parseExpr
  char ')'
  return $ ImproperList xs x

parseVector :: Parser SchemeVal
parseVector = do
  string "#("
  xs <- sepBy parseExpr spaces
  char ')'
  return $ Vector $ list2array xs

parseBytevector :: Parser SchemeVal
parseBytevector = do
  string "#u8("
  xs <- sepBy byte spaces
  char ')'
  return $ Bytevector $ list2array xs

parseQuote :: Parser SchemeVal
parseQuote = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseUnquote :: Parser SchemeVal
parseUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseQuasiquote :: Parser SchemeVal
parseQuasiquote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

-- UTILS -----------------------------------------------------------------------

tryAll = foldl1 (<|>) . map try

symbol = oneOf "!$%&|*+-/:<=>?@^_~"

hexChar = do
  string "\\x"
  x <- many1 hexDigit
  char ';'
  return $ chr $ fromHex x

escChar = do
  char '\\'
  x <- oneOf "\\\"|abnrt"
  return $ case x of
    '\\' -> x
    '"'  -> x
    '|'  -> '|'
    'a'  -> '\a'
    'b'  -> '\b'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

specialChars = [("alarm", '\a'), ("backspace", '\b'), ("delete", '\DEL'),
                ("escape", '\ESC'), ("newline", '\n'), ("null", '\NUL'),
                ("return", '\r'), ("space", ' '), ("tab", '\t')]

int = read <$> many1 digit

hex = fmap fromHex $ string "#x" >> many1 hexDigit
dec = fmap fromDec $ string "#d" >> many1 decDigit
oct = fmap fromOct $ string "#o" >> many1 octDigit
bin = fmap fromBin $ string "#b" >> many1 binDigit

fromHex = fst . head . readHex
fromDec = read
fromOct = fst . head . readOct
fromBin = foldl1 (\ a b -> 2 * a + b) . map digitToInt

decDigit = digit

binDigit = oneOf "01"

fromNumber :: SchemeVal -> Double
fromNumber (Real x) = x
fromNumber (Integer x) = fromIntegral x

list2array xs = listArray (0, length xs - 1) xs

byte = do
  x <- read <$> many1 digit
  if 0 <= x && x < 256 then return x else fail $ show x ++ " is not a byte"
