module Types
    ( LispExp(..)
    , showType )
    where

data LispExp = LInt Integer
             | LBool Bool
             | LString String
             | LList [LispExp]
             | LSymbol String
             | LFunction String
             | LBind String
             | LNil
             deriving (Eq)

instance Show LispExp where
    show (LInt x) = show x
    show (LBool True) = "#t"
    show (LBool False) = "#f"
    show (LList x) = "(" ++ (unwords $ map show x) ++ ")"
    show (LString x) = x
    show _ = ""



showType :: LispExp -> String
showType (LInt _)      = "Int"
showType (LBool _)     = "Bool"
showType (LString _)   = "String"
showType (LList _)     = "List"
showType (LSymbol _)   = "Symbol"
showType (LFunction _) = "Function"
showType (LNil)        = "Nil"
showType (LBind _)     = "SpecialFunction"

