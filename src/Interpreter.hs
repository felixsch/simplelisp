{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as M

import Types
import Parser

type LookupTable = M.Map String LispExp

data Enviroment = Enviroment
  { _globals :: LookupTable
  , _builtin :: LookupTable
  , _ctx :: [LookupTable]
  , _envbuf :: [String]
  }

makeLenses ''Enviroment

type EnvError = ErrorT String IO
type Env a = StateT Enviroment EnvError a

findSymbol :: String -> Env LispExp
findSymbol sym = do
        env <- get
        let all = env^.globals : (reverse $ env^.ctx)
        case find all (M.lookup sym $ env^.builtin) of
            Just x -> return x
            Nothing -> throwError $ "Could not find symbol'" ++ sym ++ "'"
    where
        find [] _ = Nothing
        find _ (Just x) = Just x
        find (x:xs) Nothing = find xs (M.lookup sym x)


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
-}

concat_ :: [LispExp] -> Env LispExp
concat_ = foldM cons (LString "")
    where
        cons (LString a) (LString b) = return $ LString (a ++ b)
        cons (LString a)  b          =  throwError $  "Value '" ++ show b ++ "' is not a string."




builtinTable :: M.Map String ([LispExp] -> Env LispExp)
builtinTable = M.fromList [ ("concat", concat_)]


evaluate :: LispExp -> Env LispExp
evaluate (LSymbol s) = findSymbol s
evaluate exp         = return exp







