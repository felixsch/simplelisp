module Interpreter.Eval where

import Control.Applicative
import Control.Lens

import Control.Monad.State
import Control.Monad.Error


import qualified Data.Map as M

import Types
import Interpreter.Repl
import Interpreter.Types

find :: (Ord k) => k -> [M.Map k a] -> Maybe a -> Maybe a
find _ [] _ = Nothing
find _ _ (Just x) = Just x
find s (x:xs) Nothing = find s xs (M.lookup s x)


findSymbol :: String -> Ctx LispExp
findSymbol sym = do
    env <- get
    let all = env^.globals.symbols : (reverse $ env^..ctx.traversed.symbols) -- : (reverse $ env^.ctx)
    case find sym all Nothing of
        Just x -> return x
        Nothing -> throwError $ "Could not find symbol '" ++ sym ++ "'"

findFunction :: String -> Ctx Function
findFunction sym = do
    env <- get
    let all = env^.globals.func : (reverse $ env^..ctx.traversed.func)
    case find sym all Nothing of
        Just x -> return x
        Nothing -> throwError $ "Could not find function '" ++ sym ++ "'"


evaluate :: LispExp -> Ctx LispExp
evaluate (LSymbol sym)  = findSymbol sym
evaluate (LList (x:xs)) = evalList xs =<< evaluate x
evaluate x              = return x

evalList :: [LispExp] -> LispExp -> Ctx LispExp
evalList a (LFunction f) = do
    func <- findFunction f
    args <- mapM evaluate a
    func args
evalList a (LBind f)     = do
    func <- findFunction f
    func a