module Interpreter where

import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as M
    

import Types
import Parser

type LookupTable = M.Map String LispExp

data Context = Context LookupTable (Maybe Context)

data Enviroment = Enviroment
  { envSym :: LookupTable
  , envCtx :: Maybe Context
  , envBuf :: [String]
  }

type EnvError = ErrorT String IO
type Env a = StateT Enviroment EnvError a

newCtx :: Context 
newCtx = Context M.empty Nothing


findSymbol :: String -> Env LispExp
findSymbol sym = do
    env <- get
    case M.lookup sym (envSym env) of
        Just x -> return x
        Nothing -> findSymbol' (envCtx env)
    where
        findSymbol' Nothing                    = throwError $ "Could not find symbol '" ++ sym ++ "'"
        findSymbol' (Just (Context table ctx)) = case M.lookup sym table of
                                                    Just x -> return x
                                                    Nothing -> findSymbol' ctx
                        
{-
(func 2 "moep")
(list 1 2 3 4 5)
1
2
"string"
(+ 3 4)
(defun (x y) (* x y))
#t
-}

evaluate :: LispExp -> Env LispExp
evaluate (LSymbol s) = findSymbol s
evaluate exp         = return exp







