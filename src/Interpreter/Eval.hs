module Interpreter.Eval
    ( findSymbol
    , findFunction
    , evaluate )
    where

import Control.Lens

import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as M

import Types
import Interpreter.Types



find :: (Ord k) => k -> [M.Map k a] -> Maybe a -> Maybe a

find _ _ (Just x) = Just x
find _ [] _ = Nothing
find s (x:xs) Nothing = find s xs (M.lookup s x)


findSymbol :: String -> Ctx LispExp
findSymbol sym = do
    env <- get
    let tables = (env ^.. ctx . traversed . symbols) ++ [env ^. globals . symbols]
    case find sym tables Nothing of
        Just x -> return x
        Nothing -> throwError $ "Could not find symbol '" ++ sym ++ "'"

findFunction :: String -> Ctx Function
findFunction sym = do
    env <- get
    let tables = (env ^.. ctx . traversed . func) ++ [env ^. globals . func]
    case find sym tables Nothing of
        Just x -> return x
        Nothing -> throwError $ "Could not find function '" ++ sym ++ "'"


evaluate :: LispExp -> Ctx LispExp
evaluate (LSymbol sym)  = findSymbol sym
evaluate (LList (x:xs)) = evalList xs =<< evaluate x
evaluate x              = return x

evalList :: [LispExp] -> LispExp -> Ctx LispExp
evalList a (LFunction f) = do
    func' <- findFunction f
    args <- mapM evaluate a
    liftIO $ putStrLn $ "function calling: " ++ f
    func' args
evalList a (LBind f)     = do
    func' <- findFunction f
    liftIO $ putStrLn $ "bind calling: " ++ f
    func' a
evalList _ x = throwError $ showType x ++ " is not a expression."
