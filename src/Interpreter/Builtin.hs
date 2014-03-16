module Interpreter.Builtin
    ( builtinSymbols
    , builtinFunctions )
    where

import qualified Data.Map as M

import Control.Applicative
import Control.Monad.Error
import Interpreter.Types
import Interpreter.Eval
import Types


builtinSymbols :: M.Map String LispExp
builtinSymbols = M.fromList 
    [ ("*", LFunction "mul")
    , ("mul", LFunction "mul")
    , ("add", LFunction "add")
    , ("+", LFunction "add")
    , ("list", LFunction "list")
    , ("print", LFunction "print")
    , ("if", LBind "if") ]


builtinFunctions :: M.Map String Function
builtinFunctions = M.fromList
    [ ("mul", simpleMathOp (*))
    , ("add", simpleMathOp (+))
    , ("print", lispPrint)
    , ("list", return . LList)
    , ("if", lispIf) ]



simpleMathOp :: (Integer -> Integer -> Integer) -> [LispExp] -> Ctx LispExp
simpleMathOp _  (_:[])    = throwError "Neew 2 arguments. But 1 supplied."
simpleMathOp op (x:xs)    = foldM calc x xs
    where
        calc (LInt e) (LInt t) = return $ LInt $ e `op` t
        calc (LInt _) t        = throwError $ "(E) Invalid type" ++ showType t ++ " for integer operation."
        calc t        (LInt _) = throwError $ "(E) Invalid type" ++ showType t ++ " for integer operation."
        calc _        _        = throwError "(E) Invalid types supplied to integer operation."
simpleMathOp _  []        = throwError "Need 2 arguments. But 0 supplied."


        
lispPrint :: [LispExp] -> Ctx LispExp
lispPrint x = return LNil <* mapM (liftIO . print) x


lispIf :: [LispExp] -> Ctx LispExp
lispIf args@(sw:a:b:xs)
    | length args /= 3  = throwError $ "If needs 3 arguements but " ++ show (length args) ++ " found"
    | otherwise                           = do
                                          liftIO $ putStrLn $ "Running if with: " ++ show a ++ " or " ++ show b
                                          switch <- select =<< evaluate sw
                                          evaluate (if switch then a else b)
    where
        select (LInt x)    = return $ x > 0
        select (LBool x)   = return x
        select x           = throwError $ "Invalid type in first arguement of if expression: boolean regquired but " ++ showType x ++ " found"

lispIf x = 


                                        


