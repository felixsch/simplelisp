{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import qualified Data.Map as M
import Interpreter.Types
import Interpreter.Eval
import Interpreter.Builtin
import Types
import Parser


builtins :: Lookup
builtins = Lookup builtinSymbols builtinFunctions

newLookup :: Lookup
newLookup = Lookup M.empty M.empty


newEnv :: Env
newEnv = Env builtins [] []