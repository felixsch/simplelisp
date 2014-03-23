module Interpreter.Builtin
    ( builtinSymbols
    , sampleEnv
    , builtinFunctions )
    where

import qualified Data.Map as M
import Control.Lens
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Interpreter.Types
import Interpreter.Eval
import Types

sampleEnv :: Env
sampleEnv = Env g c []
    where
        g = Lookup (M.fromList [("g_one", LInt 1), ("g_mighty", LInt 42)]) M.empty
        c = [l1, l2]
        l1 = Lookup (M.fromList [("l1_x", LBool True), ("l1_m", LSymbol "moep")]) M.empty
        l2 = Lookup (M.fromList [("l1_foo", LString "bar"), ("l1_42", LInt 42)]) M.empty

builtinSymbols :: M.Map String LispExp
builtinSymbols = M.fromList 
    [ ("*", LFunction "mul")
    , ("mul", LFunction "mul")
    , ("add", LFunction "add")
    , ("+", LFunction "add")
    , ("list", LFunction "list")
    , ("print", LFunction "print")
    , ("if", LBind "if")
    , ("quote", LBind "quote")
    , ("setq", LBind "setq")
    , ("setf", LBind "setf")
    , ("defun", LBind "defun")
    , ("show-env", LFunction "show-env")]


builtinFunctions :: M.Map String Function
builtinFunctions = M.fromList
    [ ("mul", simpleMathOp (*))
    , ("add", simpleMathOp (+))
    , ("print", lispPrint)
    , ("list", return . LList)
    , ("if", lispIf)
    , ("quote", lispQuote)
    , ("setq", lispSetQ)
    , ("setf", lispSetF)
    , ("defun", lispDefun)
    , ("show-env", showEnv)]


showEnv :: [LispExp] -> Ctx LispExp
showEnv _ = do
        env <- get
        liftIO $ print env
        return LNil


lispQuote :: [LispExp] -> Ctx LispExp
lispQuote (x:[]) = return x
lispQuote _      = throwError "quote is applied to too much arguments"


symbolsToList :: [LispExp] -> Ctx [String]
symbolsToList ((LSymbol x):xs) = (:) x <$> symbolsToList xs
symbolsToList (x:_)          = throwError $ "parameter needs to be symbol but " ++ showType x ++ " found"
symbolsToList []             = return []

fromList :: LispExp -> Ctx [LispExp]
fromList (LList x) = return x
fromList x         = throwError $ "list needed but " ++ showType x ++ " found"

lispDefun :: [LispExp] -> Ctx LispExp
lispDefun (name:params:body:[]) = do
    n <- eName name
    p <- symbolsToList =<< fromList params
    globals . symbols %= M.insert n (LFunction n)
    globals . func    %= M.insert n (fun n body p)
    return $ LFunction n
    where
        eName (LSymbol x) = return x
        eName x           = throwError $ "defun first argument needs to be symbol but " ++ showType x ++ " found"
        renderLookup _ [] [] = return $ Lookup M.empty M.empty
        renderLookup n p  a
          | length p /= length a = throwError $ n ++ " needs " ++ (show $ length p) ++ " but " ++ (show $ length a) ++ " found"
          | otherwise  = return $ Lookup (M.fromList $ zip p a) M.empty

        fun n b p a = do
            nLookup <- renderLookup n p a
            ctx %= (:) nLookup
            result <- evaluate b
            ctx %= drop 1
            return result
lispDefun x = throwError $ "defun needs 3 arguments but " ++ (show $ length x) ++ " found"




lispSetF :: [LispExp] -> Ctx LispExp
lispSetF (name:value:[]) = do
    n <- eName name
    ctx . _head . symbols %= M.insert n value 
    return value
    where 
        eName (LSymbol x) = return x
        eName x           = throwError $ "setf first argument needs to a symbol but " ++ showType x ++ " found"
{-
    where

        add n []     = M.fromList [(n, value)]
        add n x      = M.insert n value x
        eName (LSymbol x) = return x
        eName x           = throwError $ "setf first argument needs to a symbol but " ++ showType x ++ " found" -}

lispSetQ :: [LispExp] -> Ctx LispExp
lispSetQ (name:value:[]) = do
    n <- eName name
    globals . symbols %= M.insert n value
    return value
    where
        eName (LSymbol x) = return x
        eName x           = throwError $ "setq first argument needs to be a symbol but " ++ showType x ++ " found"
lispSetQ x                = throwError $ "setq needs 2 arguments but " ++ (show $ length x) ++ " found."
    


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

                                        


