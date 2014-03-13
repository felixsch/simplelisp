module Interpreter.Builtin
    ( builtinSymbols
    , builtinFunctions )
    where

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
simpleMathOp _  (_:[])    = throwError "Neew 2 arguments. But 1 supplied."
simpleMathOp op (x:xs)    = foldM calc x xs
    where
        calc (LInt e) (LInt t) = return $ LInt $ e `op` t
        calc (LInt _) t        = throwError $ "(E) Invalid type" ++ showType t ++ " for integer operation."
        calc t        (LInt _) = throwError $ "(E) Invalid type" ++ showType t ++ " for integer operation."
        calc _        _        = throwError "(E) Invalid types supplied to integer operation."
simpleMathOp _  []        = throwError "Need 2 arguments. But 0 supplied."





