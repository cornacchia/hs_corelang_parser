module ParseProg where
import Control.Applicative
import Parse

type Name = String

data Expr a
  = EVar Name               -- variables
  | ENum Int                -- numbers
  | EConstr Int Int         -- constructor tag arity
  | EAp (Expr a) (Expr a)   -- applications
  | ELet                    -- let(recursive) expressions
      IsRec                 -- NonRecursive | Recursive
      [Def a]               -- definitions
      (Expr a)              -- body
  | ECase                   -- case expression
      (Expr a)              -- expression to scrutinise
      [Alter a]             -- alternatives
  | ELam [a] (Expr a)       -- lambda abstractions
  deriving Show

type Def a = (a, Expr a)

type Alter a = (Int, [a], Expr a)
data IsRec = NonRecursive | Recursive
  deriving (Show, Eq)

type Program a = [ScDef a]

type ScDef a = (Name, [a], Expr a)

reserved :: String -> Bool
reserved s = elem s reservedWords
  where reservedWords = ["let", "letrec", "in", "case", "of", "Pack"]

-- ### Numbers
-- num -> digit1 ... digitn (n >= 1)

num :: Parser (Expr Name)
num = do n <- nat
         return (ENum n)

parseNum :: Parser (Expr Name)
parseNum = token num

-- ### Variables
-- var -> alpha varch1 ... varchn (n >= 0)
-- alpha -> an alphabetic character
-- varch -> alpha | digit | _
varch :: Parser Char
varch = letter <|> digit <|> char '_'

varName :: Parser String
varName = do a <- letter
             vs <- many varch
             if reserved (a:vs) then empty else return (a:vs)

var :: Parser (Expr Name)
var = do v <- varName
         return (EVar v)

parseVar :: Parser (Expr Name)
parseVar = token var

-- ### Alternatives
-- alts -> alt1; ... altn (n >= 1)
-- alt -> <num> var1 ... varn -> expr (n >= 0)
-- parseAlt :: Parser (Alter Name)

alt :: Parser (Alter Name)
alt = do char '<'
         n <- nat
         char '>'
         vars <- many (token varName)
         symbol "->"
         expr <- parseExpr
         return (n, vars, expr)

alts :: Parser [Alter Name]
alts = do a <- token alt
          do symbol ";"
             as <- alts
             return (a:as)
           <|> return [a]

-- ### Definitions
-- defns -> defn1; ...; defnn (n >= 1)
-- defn -> var = expr
-- parseDef :: Parser (Def Name)

defn :: Parser (Def Name)
defn = do v <- token varName
          symbol "="
          expr <- parseExpr
          return (v, expr)

defns :: Parser [Def Name]
defns = do d <- token defn
           do symbol ";"
              ds <- defns
              return (d:ds)
            <|> return [d]

-- ## Expressions
-- expr -> let defns in epxr
--      | letrec defns in expr
--      | case expr of alts
--      | \ var1 ... varn . expr (n >= 1)
--      | expr1
-- expr1 -> expr2 | expr1
--       | expr2
-- expr2 -> expr3 & expr2
--       | expr3
-- expr3 -> expr4 relop expr4
--       | expr4
-- expr4 -> expr5 + expr4
--       | expr5 - expr5
--       | expr5
-- expr5 -> expr6 * expr5
--       | expr6 / expr5
--       | expr6
-- expr6 -> aexpr1 ... aexprn (n >= 1)
-- aexpr -> var
--       | num
--       | Pack{num, num}
--       | ( expr )

parsePack :: Parser (Expr Name)
parsePack = do symbol "Pack"
               symbol "{"
               n1 <- natural
               symbol ","
               n2 <- natural
               symbol "}"
               return (EConstr n1 n2)

parseParensExpr :: Parser (Expr Name)
parseParensExpr = do symbol "("
                     e <- parseExpr
                     symbol ")"
                     return e

parseAExpr :: Parser (Expr Name)
parseAExpr = parseVar <|> parseNum <|> parsePack <|> parseParensExpr

parseLet :: Parser (Expr Name)
parseLet = do symbol "let"
              defs <- defns
              symbol "in"
              e <- parseExpr
              return (ELet NonRecursive defs e)

parseLetRec :: Parser (Expr Name)
parseLetRec = do symbol "letrec"
                 defs <- defns
                 symbol "in"
                 e <- parseExpr
                 return (ELet Recursive defs e)

parseCase :: Parser (Expr Name)
parseCase = do symbol "case"
               e <- parseExpr
               symbol "of"
               alternatives <- alts
               return (ECase e alternatives)

parseLambda :: Parser (Expr Name)
parseLambda = do token (sat (== '\\'))
                 vs <- some (token varName)
                 symbol "."
                 e <- parseExpr
                 return (ELam vs e)

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do e1 <- parseAExpr
                do e2 <- parseExpr6
                   return (EAp e1 e2)
                 <|> return e1

parseMult :: Parser (Expr Name)
parseMult = do e1 <- parseExpr6
               symbol "*"
               e2 <- parseExpr5
               return (EAp (EAp (EVar "*") e1) e2)

parseDiv :: Parser (Expr Name)
parseDiv = do e1 <- parseExpr6
              symbol "/"
              e2 <- parseExpr5
              return (EAp (EAp (EVar "/") e1) e2)

parseExpr5 :: Parser (Expr Name)
parseExpr5 = parseMult <|> parseDiv <|> parseExpr6

parseSum :: Parser (Expr Name)
parseSum = do e1 <- parseExpr5
              symbol "+"
              e2 <- parseExpr4
              return (EAp (EAp (EVar "+") e1) e2)

parseSub :: Parser (Expr Name)
parseSub = do e1 <- parseExpr5
              symbol "-"
              e2 <- parseExpr5
              return (EAp (EAp (EVar "-") e1) e2)

parseExpr4 :: Parser (Expr Name)
parseExpr4 = parseSum <|> parseSub <|> parseExpr5

parseRelop :: Parser (Expr Name)
parseRelop = do e1 <- parseExpr4
                symbol "relop"
                e2 <- parseExpr4
                return (EAp (EAp (EVar "relop") e1) e2)

parseExpr3 :: Parser (Expr Name)
parseExpr3 = parseRelop <|> parseExpr4

parseAnd :: Parser (Expr Name)
parseAnd = do e1 <- parseExpr3
              symbol "&"
              e2 <- parseExpr2
              return (EAp (EAp (EVar "&") e1) e2)

parseExpr2 :: Parser (Expr Name)
parseExpr2 = parseAnd <|> parseExpr3

parseOr :: Parser (Expr Name)
parseOr = do e1 <- parseExpr2
             symbol "|"
             e2 <- parseExpr1
             return (EAp (EAp (EVar "|") e1) e2)

parseExpr1 :: Parser (Expr Name)
parseExpr1 = parseOr <|> parseExpr2

parseExpr :: Parser (Expr Name)
parseExpr = parseLet <|> parseLetRec <|> parseCase <|> parseLambda <|> parseExpr1

-- ### Supercombinators
-- sc -> var var1 ... varn = expr (n >= 0)

parseScDef :: Parser (ScDef Name)
parseScDef = do v <- token varName
                pf <- many (token varName)
                symbol "="
                body <- parseExpr
                return (v, pf, body)

-- ### Program
-- program -> sc1; ... scn (n >= 1)
parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do symbol ";"
                  ps <- parseProg
                  return (p:ps)
                <|> return [p]