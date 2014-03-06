module Types where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error


data LispExp = LInt Integer
             | LBool Bool
             | LString String
             | LList [LispExp]
             | LSymbol String
             | LFunction String
             | LBind String
             deriving (Show, Eq)



showType :: LispExp -> String
showType (LInt _)      = "Int"
showType (LBool _)     = "Bool"
showType (LString _)   = "String"
showType (LList _)     = "List"
showType (LSymbol _)   = "Symbol"
showType (LFunction _) = "Function"
showType (LBind _)     = "SpecialFunction"

