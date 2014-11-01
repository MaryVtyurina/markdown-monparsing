module Parsers where

import Control.Monad
import Data.Char(ord)

newtype Parser a = Parser (String -> [(a,String)])

parse :: Parser p -> String -> [(p,String)]
parse (Parser p) = p

instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                            (a,cs') <- parse p cs])

instance MonadPlus Parser where
  mzero = Parser (\cs -> [])
  p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)

-- Условимся далее, что to consume == кушать

-- |Кушает один произвольный символ
item :: Parser Char
item = Parser (\inp -> case inp of
                        [] -> []
                        (x:xs) -> [(x,xs)])

-- |Разборчиво кушает символ, только если тот удовлетворяет предикату
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item 
  if p x then return x else mzero

----------------Парсеры для одиночных символов----------------

-- |Куашет символ только в том случае, если он совпадает с указанным
char :: Char -> Parser Char
char x = sat (\y -> x == y)

-- |Decimal digit
digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

-- |Lowercase letter
lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

-- |Uppercase letter
upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

-- |Anycase letter
letter :: Parser Char
letter = lower `mplus` upper

-- |Anycase letter or decimal digit
alphanum :: Parser Char
alphanum = letter `mplus` digit
----------------Парсеры для групп символов----------------

-- |Word (string of letters)
word :: Parser String
word = neWord `mplus` return ""
  where
    neWord = do
      x <- letter
      xs <- word
      return (x:xs)

-- |Applyes parser p many times
many :: Parser a -> Parser [a]
many p = many1 p `mplus` return []

many1 :: Parser a -> Parser [a]
many1 p = do 
  a <- p
  as <- many p
  return (a:as)

-- |Parse a natural number
nat :: Parser Int
nat = do
  xs <- many1 digit
  return $ eval xs
    where
      eval xs = foldl1 op (map (\x -> ord x - ord '0') xs) 
      m `op` n = 10*m + n

-- |Parse a specified string
string :: String -> Parser String
string "" = return ""
string (c:cs) = do 
  char c
  string cs
  return (c:cs)

-- |Parse a token with specific parser, thow away any trailing spaces
token :: Parser a -> Parser a
token p = do 
  a <- p 
  spaces
  return a

-- |Parse a symbolic token, just a specification of token parser
symbol :: String -> Parser String
symbol cs = token (string cs)

-- |Parse identifier, i.e. word with leading lowercase letters and trailing alphanums 
ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

-- |Same as ident but fails if argument is a member of list of keywords
identifier :: [String] -> Parser String
identifier kwords = do
  x <- ident
  if elem x kwords then return "" else return x

-- |Parse a thing enclosed by brackets
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do 
  open
  x <- p
  close
  return x

----------------"Lexical issues"----------------
spaces :: Parser String
spaces = many (sat isSpace)
  where 
    isSpace = (\x -> x == ' ' || x == '\n' || x == '\t')

----------------Special parsers----------------
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) `mplus` return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do 
  a <- p
  rest a
    where
        rest a = (do f <- op
                     b <- p
                     rest (f a b))
                  `mplus` return a


