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
data isRec = NonRecursive | Recursive
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

preludeDefs :: CoreProgram
preludeDefs = [
  ("I", ["x"], EVar "x"),                                            -- identity: I x = x
  ("K", ["x","y"], EVar "x"),                                        -- K x y = x
  ("K1",["x","y"], EVar "y"),                                        -- K1 x y = y
  ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))               -- S f g x = f x (g x)
                           (EAp (EVar "g") (EVar "x"))),
  ("compose", ["f","g","x"], EAp (EVar "f")                          -- compose f g x = f (g x)
                                 (EAp (EVar "g") (EVar "x"))),
  ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) -- twice f = compose f f
]

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do character ';'
                  ps <- parseProg
                  return (p:ps)
                  <|> return [p]

parseScDef :: Parser (ScDefn Name)
parseScDef = do v <- parseVar
                pf <- many parseVar
                character '='
                body <- parseExpr
                return (v, pf, body)

parseExpr :: Parser (Expr Name)
parseAExpr :: Parser (Expr Name)
parseDef :: Parser (Def Name)
parseAlt :: Parser (Alter Name)