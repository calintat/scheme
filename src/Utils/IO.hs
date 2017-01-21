module Utils.IO where

import Control.Arrow
import Control.Monad.Except
import System.IO

import Core.Error
import Core.Language

import Repl.Eval

import Utils.Storage

ioPrimitives :: [(String, SchemeVal)]
ioPrimitives = (map . second) IOPrimitive ioPrimitivesList

ioPrimitivesList = [("apply", applyProc),
                    ("open-input-file", makePort ReadMode),
                    ("open-output-file", makePort WriteMode),
                    ("close-input-port", closePort),
                    ("close-output-port", closePort),
                    ("read", read1),
                    ("read-all", readAll),
                    ("read-contents", readContents),
                    ("write", write)]

applyProc :: [SchemeVal] -> IOThrowsError SchemeVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

readAll :: [SchemeVal] -> IOThrowsError SchemeVal
readAll [String filename] = List <$> load filename

makePort :: IOMode -> [SchemeVal] -> IOThrowsError SchemeVal
makePort mode [String file] = fmap IOPort $ liftIO $ openFile file mode

closePort :: [SchemeVal] -> IOThrowsError SchemeVal
closePort [IOPort port] = liftIO $ hClose port >> return (Boolean True)
closePort _             = return $ Boolean False

read1 :: [SchemeVal] -> IOThrowsError SchemeVal
read1 []            = read1 [IOPort stdin]
read1 [IOPort port] = liftIO (hGetLine port) >>= liftThrows . readExpr

write :: [SchemeVal] -> IOThrowsError SchemeVal
write [obj]              = write [obj, IOPort stdout]
write [obj, IOPort port] = liftIO $ hPrint port obj >> return (Boolean True)

readContents :: [SchemeVal] -> IOThrowsError SchemeVal
readContents [String file] = String <$> liftIO (readFile file)
