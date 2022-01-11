module Calculator where

import Control.Applicative
import Control.Monad
import NanoParsec

{-
  number = [ ”-” ] digit { digit }.
  digit = ”0” | ”1” | ... | ”8” | ”9”.
  expr = term { addop term }.
  term = factor { mulop factor }.
  factor = ”(” expr ”)” | number.
  addop = ”+” | ”-”.
  mulop = ”*”.
-}

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving stock (Show)

eval :: Expr -> Int
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Lit n) = n

int :: Parser Expr
int = fmap Lit number

expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addOp :: Parser (Expr -> Expr -> Expr)
addOp = infixOp "+" Add <|> infixOp "-" Sub

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = infixOp "*" Mul

run :: String -> Expr
run = runParser expr
