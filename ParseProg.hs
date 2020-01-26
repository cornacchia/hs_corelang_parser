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

-- tag, list bound variables, rhs expression
type Alter a = (Int, [a], Expr a)
data IsRec = NonRecursive | Recursive
  deriving Show
type CoreAlt = Alter Name

-- this function is used to identify expressions with no internal structure
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _ = False

type CoreExpr = Expr Name

-- extract variable names from a list of definitions
bindersOf :: [(a, b)] -> [a]
bindersOf definitions = [name | (name, rhs) <- definitions]

-- extract right hand sides from a list of definitions
rhhsOf :: [(a, b)] -> [b]
rhhsOf definitions = [rhs | (name, rhs) <- definitions]

-- a Core program is a list of supercombinator definitions
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- name of the supercombinator, arguments, body
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

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
             return (a:vs)

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
-- expr -> expr aexpr
--      | expr1 binop expr2
--      | let defns in epxr
--      | letrec defns in expr
--      | case expr of alts
--      | \ var1 ... varn . expr (n >= 1)
--      | aexpr
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
parseLambda = do symbol "\\"
                 vs <- some varName
                 symbol "."
                 e <- parseExpr
                 return (ELam vs e)

parseExpr :: Parser (Expr Name)
parseExpr = parseLet <|> parseLetRec <|> parseCase <|> parseLambda <|> parseAExpr

parseScDef :: Parser (ScDefn Name)
parseScDef = do v <- token varName
                pf <- many (token varName)
                symbol "="
                body <- parseExpr
                return (v, pf, body)

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do symbol ";"
                  ps <- parseProg
                  return (p:ps)
                <|> return [p]