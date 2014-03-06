module Interpreter.Builtin where

import qualified Data.Map as M

import Control.Monad.Error
import Interpreter.Types
import Types


builtinSymbols :: M.Map String LispExp
builtinSymbols = M.fromList 
    [ ("*", LFunction "mul")
    , ("+", LFunction "add")
    , ("list", LFunction "list") ]


builtinFunctions :: M.Map String Function
builtinFunctions = M.fromList
    [ ("mul", simpleMathOp (*))
    , ("add", simpleMathOp (+))
    , ("list", return . LList) ]



simpleMathOp :: (Integer -> Integer -> Integer) -> [LispExp] -> Ctx LispExp
simpleMathOp op (x:xs) = foldM calc x xs
    where
        calc (LInt e) (LInt x) = return $ LInt $ e `op` x
        calc (LInt e) x        = throwError $ "(E) Invalid type" ++ showType x ++ " for integer operation."
        calc x        (LInt e) = throwError $ "(E) Invalid type" ++ showType x ++ " for integer operation."
        calc _        _        = throwError $ "(E) Not enough arguments"





