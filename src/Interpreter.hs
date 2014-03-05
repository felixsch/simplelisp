{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as M

import Types
import Parser

data Lookup = Lookup
    { _symbols :: M.Map String LispExp
    , _func    :: M.Map String ([LispExp] -> Ctx LispExp) }

data Env = Env
  { _globals :: Lookup
  , _builtin :: Lookup
  , _ctx :: [Lookup]
  , _envbuf :: [String]
  }

type CtxError = ErrorT String IO
type Ctx a = StateT Env CtxError a

makeLenses ''Lookup
makeLenses ''Env


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



{-
findSymbol :: String -> Env LispExp
findSymbol sym = do
    env <- get

    case M.lookup sym (envSym env) of
        Just x -> return x
        Nothing -> findSymbol' (envCtx env)
    where
        findSymbol' Nothing                    = throwError $ "Could not find symbol '" ++ sym ++ "'"
        findSymbol' (Just (Ctx table ctx)) = case M.lookup sym table of
                                                    Just x -> return x
                                                    Nothing -> findSymbol' ctx


(func 2 "moep")

(list 1 2 3 4 5)
1
2
"string"
(+ 3 4)
(defun (x y) (* x y))
#t
j

concat_ :: [LispExp] -> Env LispExp
concat_ = foldM cons (LString "")
    where
        cons (LString a) (LString b) = return $ LString (a ++ b)
        cons (LString a)  b          =  throwError $  "Value '" ++ show b ++ "' is not a string."




builtinTable :: M.Map String ([LispExp] -> Env LispExp)
builtinTable = M.fromList [ ("concat", concat_)]

-}







