module Parser where


import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)


import Types

(<:>) a b = (:) <$> a <*> b

ignore :: Parser ()
ignore = many (wspaces <|> comments) *> pure ()
  where
    wspaces = many $ oneOf " \n"
    comments = char ';' <:> many anyChar

-- see: https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
pInt :: Parser LispExp
pInt = LInt <$> (read' <$> (plus <|> minus <|> number))
     where
         read' = read :: String -> Integer
         plus  = char '+' *> number
         minus = char '-' <:> number
         number = many1 digit

pString :: Parser LispExp
pString = LString <$> (char '"' *> many anyChar <* char '"')

pBool :: Parser LispExp
pBool = LBool <$> choice [ string "#t" *> return True,
                           string "#f" *> return False ]

pSymbol :: Parser LispExp
pSymbol = LSymbol <$> (special <|> symbol)
    where
        special = char '*' <:> symbol
        symbol  = many1 $ letter <|> digit

pList :: Parser LispExp
pList = LList <$> (ignore *> char '(' *> many pExp <* char ')' <* ignore)


pExp :: Parser LispExp
pExp =   pInt
     <|> pString
     <|> pSymbol
     <|> pList

parseLisp :: String -> Either ParseError LispExp
parseLisp = parse pExp "" 


