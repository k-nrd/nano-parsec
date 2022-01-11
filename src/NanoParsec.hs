{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Control.Applicative
import Control.Monad

newtype Parser a = Parser {parse :: String -> [(a, String)]}

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

-- Lets us apply second parser to the result of the first parser
instance Applicative Parser where
  pure = unit
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

-- Lets us compose parsers
instance Monad Parser where
  (>>=) = bind

-- Lets us concat parser results
instance MonadPlus Parser where
  mzero = failure
  mplus = combine

-- Get result of the first parser, or, if it fails, the second one
instance Alternative Parser where
  empty = mzero
  (<|>) = option

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, _)] -> error "Parser did not consume the entire stream."
    _ -> error "Parser error."

-- Advances the parser over the string
item :: Parser Char
item = Parser $
  \case
    [] -> []
    (c : cs) -> [(c, cs)]

-- Compose parser operation with the result of another parse
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s ->
  concatMap (\(a, s') -> parse (f a) s') (parse p s)

-- Injects a single value as result without reading from stream
unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

-- Combines two parsers unconditionally
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

-- Represents an "empty" parser
failure :: Parser a
failure = Parser (const [])

-- Returns parser composed with the second parser if the first one fails, else it composes with the first parser
option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    res -> res

-- Takes parser that matches value, returns a parser that matches a list of values (at least 1)
some :: Alternative f => f a -> f [a]
some v = someV
  where
    manyV = someV <|> pure []
    someV = (:) <$> v <*> manyV

-- Takes parser that matches value, returns a parser that matches a list of values (at least 0)
many :: Alternative f => f a -> f [a]
many v = manyV
  where
    manyV = someV <|> pure []
    someV = (:) <$> v <*> manyV

-- Takes a predicate, returns a parser that matches values that satisfy the predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  item >>= \c ->
    if p c
      then unit c
      else Parser (const [])

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

char :: Char -> Parser Char
char c = satisfy (== c)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

-- Parses one or more occurences of `p` separated by `op`,
-- recursing until failure on the left hand side of the stream.
-- Can be used to parse left-recursive grammars.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  a <- p
  rest a
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        <|> return a

isDigit :: Char -> Bool
isDigit x = x `elem` ['0' .. '9']

natural :: Parser Integer
natural = read <$> NanoParsec.some digit

string :: String -> Parser String
string [] = return []
string (c : cs) = do
  char c
  string cs
  return (c : cs)

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = NanoParsec.many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- NanoParsec.some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n
