-- Code from Monadic Parser Combinator
{-#LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveFunctor, MonadComprehensions, InstanceSigs, ScopedTypeVariables,
            FunctionalDependencies, UndecidableInstances #-}

module ParserCombinators where

import Control.Monad ((>>=), return, guard)
import Control.Applicative ((<|>))
import Prelude
import States_Readers
---------------------------------------------------------------------------
--                               Parser type
---------------------------------------------------------------------------

type Pstring  = (Pos,String)
type Pos      = (Int,Int) -- (line, column)

type Parser a = ReaderM (StateM [] Pstring) Pos a

-- Just a friendly reminder:
-- newtype ReaderM m s a = ReaderM (s -> m a)
-- newtype StateM m s a = StateM (s -> m (a,s))
-- ReaderM (StateM [] Pstring) Pos a    ~
--     Pos -> StateM [] Pstring a       ~
--     Pos -> Pstring -> [(a, Pstring)]

---------------------------------------------------------------------------
--                    Basic parser combinators
---------------------------------------------------------------------------
-- item successfully consumes the first character if
-- the input string is non-empty, and fails if the position of the character to be consumed
-- is not onside with respect to current definition position

item  = p <|> zero where
        p = do
            (pos, inp) <- update newstate
            guard $ not $ null inp
            defpos     <- env
            guard $ onside pos defpos
            return $ head inp

--A position is onside if its column number is strictly greater
--than the current definition column
onside :: Pos -> Pos -> Bool
onside (l,c) (dl,dc) = (c > dc) || (l == dl)

--Takes the first character from the input string,
--and updates the current position
newstate :: Pstring -> Pstring
newstate ((l,c),x:xs)
    = (newpos,xs)
        where
            newpos = case x of
                '\n' -> (l+1,0)
                '\t' -> (l,((c `div` 8)+1)*8)
                _    -> (l,c+1)

--The first result produced by certain parsers
first :: Parser a -> Parser a
first p = ReaderM f where
        arg pos pstr = unS (unR p pos) pstr
        f pos = StateM $ \pstr ->
            -- let
            --     arg = unS (unR p pos) pstr in
                if null $ arg pos pstr then [] else [head $ arg pos pstr]

--it takes smth, applies both argument parsers to this smth, and concatenates the resulting lists(if p succeeds then q is never applied)
(+++)  :: Parser a -> Parser a -> Parser a
p +++ q = first (p <|> q)


string :: String -> Parser String
string ""     = return ""
string (x:xs) = char x    >>= \_ ->
                   string xs >>= \_ ->
                   return (x:xs)

char  :: Char -> Parser Char
char x = sat (\y -> x == y)

-- always fails, regardless of the input string
zero :: Parser a
zero = ReaderM f where
        f pos = StateM $ \pstr -> []

-- takes a predicate (a Boolean valued function),
-- and yields a parser that consumes a single character if it satisfies the predicate
-- and fails otherwise
sat  :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
   if p x then return x else zero

-- applies a parser p zero or more times to an input string
many  :: Parser a -> Parser [a]
many p = many1 p <|> return []

--non-empty sequences of items
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- removes junk after applying a parser
token  :: Parser a -> Parser a
token p = do
            v <- p
            _ <- junk
            return v
--
digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

symbol :: String -> Parser String
symbol xs = token (string xs)

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

plus :: Parser a -> Parser a -> Parser a
plus = (+++)

letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum  = letter `plus` digit

--parser for identifiers (lower-case letter followed by zero or more alpha-numeric characters)
ident :: Parser String
ident  = do
            x <- lower
            xs <- many alphanum
            return $ x : xs

-- keyword check (takes a list of keywords as an argument)
-- keyword is a string that is not permitted as an identifier
identifier :: [String] -> Parser String
identifier ks = do
                  x <- (first ident)
                  guard $ not (elem x ks)
                  token $ return x

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
      _ <- open
      x <- p
      _ <- close
      return x

--  like many1, but instances of p are separated by a parser sep whose result values are ignored
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = do
                  x <- p
                  xs <- many f
                  return (x:xs)
                      where
                          f = do
                                  _ <- sep
                                  y <- p
                                  return y

-- parses non empty sequences of items separated by operators that associate to the right, rather than to the left
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = rec <|> p where
      rec = do
                  x <- p
                  f <- op
                  y <- p `chainr1` op
                  return $ f x y

wspaceOrTab = (\x -> x == ' ' || x == '\t')

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest where
  rest x = one x <|> return x
  one x = do
      f <- op
      y <- p
      rest (f x y)

comment :: Parser ()
comment = [() | _ <- string "--"
            , _ <- many (sat (\x -> x /= '\n'))]

spaces :: Parser ()
spaces = [() | _ <- many1 (sat isSpace)]
    where
        isSpace x =
            (x == ' ') || (x == '\n') || (x == '\t')

junk :: Parser ()
junk  = setenv (0, -1) (many (spaces +++ comment)) >> return ()

--Setting the definition position locally for
--each new definition in the sequence
off  :: Parser a -> Parser a
off p = [v | (dl, dc)    <- env
           , ((l, c), _) <- fetch
           , c == dc
           , v           <- setenv (l, dc) p]

-- Combinator that parses a sequence of definitions subject
-- to the Gofer offside rule
many1_offside  :: Parser a -> Parser [a]
many1_offside p = [vs | (pos, _) <- fetch
                      , vs       <- setenv pos (many1 (off p))]

--Can also parse an empty sequence of definitions
many_offside :: Parser a -> Parser [a]
many_offside p = many1_offside p <|> return []

parse p s = unS (unR p (1,0)) ((1,0), s)

-- parse :: TM.TextualMonoid t =>
--   Parser t a -> t -> Either (ErrorReport t) (a,ParserState t)
-- parse (Parser p) s =
--   runStateT p (ParserState {remainder = s, position = initPos})
--     where initPos = (1,1)
