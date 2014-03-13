module Parser (parseLisp) where


import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)


import Types

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

wspaces :: Parser String
wspaces = many $ oneOf " \n"

comments :: Parser String
comments = char ';' <:> many anyChar

-- see: https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
pInt :: Parser LispExp
pInt = LInt <$> (read' <$> (plus <|> minus <|> number))
     where
         read' = read :: String -> Integer
         plus  = try $ char '+' *> number
         minus = try $ char '-' <:> number
         number = many1 digit

pString :: Parser LispExp
pString = fmap LString $ (char '"' *> many (noneOf "\"") <* char '"')

pBool :: Parser LispExp
pBool = LBool <$> choice [ string "#t" *> return True,
                           string "#f" *> return False ]

pSymbol :: Parser LispExp
pSymbol = LSymbol <$> (liftA2 (:) (letter <|> symbol) $ many (letter <|> symbol <|> digit))
    where
        symbol  = oneOf "-+=!|&/*<>"

pList :: Parser LispExp
pList = LList <$> (ignore *> char '(' *> many expr <* char ')' <* ignore)
    where
        ignore = wspaces <|> comments
        expr   = wspaces *> pExp


pExp :: Parser LispExp
pExp =   pInt
     <|> pBool
     <|> pString
     <|> pSymbol
     <|> pList

parseLisp :: String -> Either ParseError [LispExp]
parseLisp = parse (many pExp) "" 


