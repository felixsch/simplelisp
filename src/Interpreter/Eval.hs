module Interpreter.Eval where

import Control.Applicative
import Control.Lens

import Control.Monad.State
import Control.Monad.Error


import qualified Data.Map as M

import Types
import Interpreter.Repl
import Interpreter.Types


findSymbol :: String -> Ctx LispExp
findSymbol sym = do
        env <- get
        let all = env^.globals.symbols : (reverse $ env^..ctx.traversed.symbols) -- : (reverse $ env^.ctx)
        case find all Nothing of
            Just x -> return x
            Nothing -> throwError $ "Could not find symbol'" ++ sym ++ "'"
    where
        find [] _ = Nothing
        find _ (Just x) = Just x
        find (x:xs) Nothing = find xs (M.lookup sym x)


evaluate :: LispExp -> Ctx LispExp
evaluate (LSymbol sym) = findSymbol sym
