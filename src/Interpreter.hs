module Interpreter where

--import Control.Monad.Error
import qualified Data.Map as M
import Interpreter.Types
import Interpreter.Builtin
import Parser


builtins :: Lookup
builtins = Lookup builtinSymbols builtinFunctions

newLookup :: Lookup
newLookup = Lookup M.empty M.empty


newEnv :: Env
newEnv = Env builtins [] []



loadFile :: FilePath -> IO ()
loadFile path = do
    content <- readFile path
    case parseLisp content of
        Left x -> putStrLn $ "Parse error:" ++ show x
        Right x -> print x

