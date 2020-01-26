module Parse where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\inp -> case parse p inp of
                  [] -> []
                  [(x, xs)] -> [(f x, xs)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\inp -> [(x, inp)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  fp <*> xp = P (\inp -> case parse fp inp of
                  [] -> []
                  [(f, xs)] -> parse (fmap f xp) xs)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f = P (\inp -> case parse px inp of
                  [] -> []
                  [(x, out)] -> parse (f x) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                [] -> parse q inp
                [(v, out)] -> [(v, out)])

item :: Parser Char
item = P (\inp -> case inp of
            [] -> []
            (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat cond = do p <- item
              if (cond p) then return p else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do c <- char x
                   cs <- string xs
                   return (c:cs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- Looks for any identifier (alphanumeric string)
identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

-- Looks for a particular symbol, ignoring whitespace
symbol :: String -> Parser String
symbol xs = token (string xs)