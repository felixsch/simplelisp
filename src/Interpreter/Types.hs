{-# LANGUAGE TemplateHaskell #-}
module Interpreter.Types
    ( Function
    , Lookup(..)
    , Env(..)
    , CtxError
    , Ctx
    , Key(..)
    , Input(..)
    , symbols
    , func
    , globals
    , ctx
    , history)
    where

import Control.Lens
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Error
import Types


type Function = ([LispExp] -> Ctx LispExp)

data Lookup = Lookup
    { _symbols :: M.Map String LispExp
    , _func    :: M.Map String Function }

data Env = Env
  { _globals :: Lookup
  , _ctx :: [Lookup]
  , _history :: [String]
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
         | KeyTab
         | KeyUnknown
         deriving (Show)

data Input = Normal String | Special Key
    deriving (Show)
    
makeLenses ''Lookup
makeLenses ''Env
