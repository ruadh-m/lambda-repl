module Main where

import Control.Applicative (Alternative, empty, (<|>), many, some)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Data.Functor (void)
import Data.List (delete, union)
import System.IO (hSetEncoding, hFlush, stdin, stdout, utf8)

--------------------------------------------------------------------------------
-- AST DECLARATION
--------------------------------------------------------------------------------

-- The abstract syntax tree for the untyped lambda calculus.
data Expr
  = Var String       -- Variables: x, y, etc.
  | Lam String Expr  -- Abstractions: λx. e
  | App Expr Expr    -- Applications: e1 e2
  deriving (Eq, Show)

-- A simple pretty-printer so we can read terms easily.
pretty :: Expr -> String
pretty (Var x)     = x
pretty (Lam x e)   = "λ" ++ x ++ "." ++ pretty e
pretty (App e1 e2) = left e1 ++ " " ++ right e2
  where
    -- Left side: parentheses only if it's a lambda
    left e = case e of
               Lam _ _ -> "(" ++ pretty e ++ ")"
               _       -> pretty e
    -- Right side: parentheses if it's a lambda or application
    right e = case e of
                Var _ -> pretty e
                _     -> "(" ++ pretty e ++ ")"

--------------------------------------------------------------------------------
-- SUBSTITUTION
--------------------------------------------------------------------------------

-- The free variables of an expression.
freeVars :: Expr -> [String]
freeVars (Var x)     = [x]
freeVars (Lam x e)   = delete x (freeVars e)
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2

-- Generate a variable name not present in the given list.
-- We append primes (') until we find an unused name.
fresh :: String -> [String] -> String
fresh x used
  | x `notElem` used = x
  | otherwise        = fresh (x ++ "'") used

-- Capture-avoiding substitution:  body[e/x]
-- Replace free occurrences of x in body with e.
subst :: String -> Expr -> Expr -> Expr
subst x e (Var y)
  | x == y    = e
  | otherwise = Var y

subst x e (Lam y body)
  | x == y    = Lam y body
  | y `elem` freeVars e =
      let y' = fresh y (freeVars e `union` freeVars body `union` [x])
      in Lam y' (subst x e (subst y (Var y') body))
  | otherwise =
      Lam y (subst x e body)

subst x e (App e1 e2) =
  App (subst x e e1) (subst x e e2)                

--------------------------------------------------------------------------------
-- PARSING
--------------------------------------------------------------------------------

-- An empty list means failure. A singleton list is the clean, unambiguous
-- result we aim for. Multiple results mean ambiguity.
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> [(f x, s') | (x, s') <- p s]

instance Applicative Parser where
  pure x = Parser $ \s -> [(x, s)]
  Parser pf <*> Parser px = Parser $ \s ->
    [(f x, s'') | (f, s') <- pf s, (x, s'') <- px s']

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \s ->
    [(y, s'') | (x, s') <- p s, (y, s'') <- runParser (f x) s']

-- Try the first parser; if it fails (empty list), try the second.
instance Alternative Parser where
  empty = Parser $ \_ -> []
  Parser p <|> Parser q = Parser $ \s ->
    case p s of
      [] -> q s
      res -> res

-- Parse a single character.
item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c, cs)]

-- Parse a character satisfying a predicate.
sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then return c else empty

-- Parse a specific character.
char :: Char -> Parser Char
char c = sat (== c)

-- Parse a specific string.
string :: String -> Parser String
string = mapM char

-- Consume zero or more whitespace characters.
spaces :: Parser ()
spaces = void $ many (sat isSpace)

-- Skip trailing whitespace after a parser
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- Parse a literal symbol (a string) and skip any following whitespace.
symbol :: String -> Parser String
symbol s = lexeme (string s)

-- Parse an identifier: a letter followed by zero or more letters/digits.
ident :: Parser String
ident = lexeme $ do
  c  <- sat isAlpha
  cs <- many (sat isAlphaNum)
  return (c:cs)

-- Parse a variable.
var :: Parser Expr
var = Var <$> ident

-- Helper: between open close p parses p between open and close.
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

-- Parse a parenthesized expression.
parens :: Parser Expr
parens = between (lexeme (char '(')) (lexeme (char ')')) expr

-- Parse an abstraction: \x. e
lam :: Parser Expr
lam = do
  void $ lexeme (char '\\')
  x  <- ident
  void $ symbol "."
  e  <- expr
  return (Lam x e)

-- Parse an "atom": a variable, a parenthesized expr, or an abstraction.
atom :: Parser Expr
atom = var <|> parens <|> lam

-- Left-associative chain: e1 e2 e3 ... becomes App (App e1 e2) e3
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = foldl (\acc (f, y) -> f acc y) <$> p <*> many ((,) <$> op <*> p)

-- Application is just juxtaposition (no operator symbol).
app :: Parser Expr
app = chainl1 atom (return App)

-- Top-level expression: application chain or a lone atom/abstraction.
expr :: Parser Expr
expr = app

-- End-of-file parser: succeeds only if the entire input is consumed.
eof :: Parser ()
eof = Parser $ \s -> if null s then [((), s)] else []

-- Run a parser on the entire input, requiring full consumption.
parseExpr :: String -> Either String Expr
parseExpr s =
  case runParser (spaces *> expr <* spaces <* eof) s of
    [(e, "")] -> Right e
    [(_, rs)] -> Left $ "Unconsumed input: " ++ show rs
    []        -> Left "Parse error"
    _         -> Left "Ambiguous parse"

-- Normalize input to fix issues with unicode encoding,
-- allowing '\', 'λ', and 'Λ' for lambdas
normalizeLambda :: String -> String
normalizeLambda = map $ \c -> case c of
  'λ' -> '\\'
  'Λ' -> '\\'
  _   -> c

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8

  putStrLn "SUBSTITUTION TESTING"
  putStrLn ""

  -- Example 1: Simple substitution (no renaming needed)
  let ex1   = Lam "x" (App (Var "x") (Var "y"))
  let sub1  = App (Var "z") (Var "w")
  putStrLn $ "Expression:  " ++ pretty ex1
  putStrLn $ "Substitute:  " ++ pretty sub1
  putStrLn $ "For var:     x"
  putStrLn $ "Result:      " ++ pretty (subst "x" sub1 ex1)
  putStrLn ""

  -- Example 2: Capture avoidance (z is free in sub1, so it must be renamed)
  let ex2 = Lam "z" (App (Var "x") (Var "z"))
  putStrLn $ "Expression:  " ++ pretty ex2
  putStrLn $ "Substitute:  " ++ pretty sub1
  putStrLn $ "For var:     x"
  putStrLn $ "Result:      " ++ pretty (subst "x" sub1 ex2)
  putStrLn ""

  -- Example 3: Variable does not occur free
  let ex3 = Lam "x" (App (Var "x") (Var "z"))
  putStrLn $ "Expression:  " ++ pretty ex3
  putStrLn $ "Substitute:  " ++ pretty sub1
  putStrLn $ "For var:     y"
  putStrLn $ "Result:      " ++ pretty (subst "y" sub1 ex3)
  putStrLn ""

  -- REPL
  putStrLn "Untyped Lambda Calculus -- Parser Test"
  putStrLn "Type an expression (e.g. \\x.x x) or 'quit' to exit."
  loop
  where
    loop = do
      putStr "λ> " >> hFlush stdout
      line <- normalizeLambda <$> getLine
      case line of
        "quit" -> putStrLn "Goodbye."
        _      -> do
          case parseExpr line of
            Left err -> putStrLn $ "Error: " ++ err
            Right e  -> do
              putStrLn $ "AST:    " ++ show e
              putStrLn $ "Pretty: " ++ pretty e
          loop
