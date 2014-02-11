module Types where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error


data LispExp = LInt Integer
             | LBool Bool
             | LString String
             | LList [LispExp]
             | LSymbol String
             | LFunction String (Maybe LispExp)
             | LBind String LispExp
             deriving (Show, Eq)

