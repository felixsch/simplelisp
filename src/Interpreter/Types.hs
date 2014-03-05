{-# LANGUAGE TemplateHaskell #-}
module Interpreter.Types where

import Control.Lens
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Error
import Types


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


data Key = KeyUp
         | KeyDown
         | KeyRight
         | KeyLeft
         | KeyPos1
         | KeyEnd
         | KeyBack
         | KeyDel
         | KeyEnter
         | KeyUnknown

data Input = Normal String | Special Key

data ReplEnv = ReplEnv
    { _env :: Env
    , _history :: [String] }

type Repl a = StateT Env IO a



makeLenses ''Lookup
makeLenses ''Env
makeLenses ''ReplEnv