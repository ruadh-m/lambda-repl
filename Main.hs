module Main where

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
pretty (App e1 e2) = paren e1 ++ " " ++ paren e2
  where
    -- Parenthesize applications and lambda expressions,
    -- preserving left-associativity
    paren e@(Lam _ _) = "(" ++ pretty e ++ ")"
    paren e@(App _ _) = "(" ++ pretty e ++ ")"
    paren e           = pretty e

main :: IO ()
main = do
  -- Church-encoded true: λt.λf.t
  let true  = Lam "t" (Lam "f" (Var "t"))
  -- Church-encoded false: λt.λf.f
  let false = Lam "t" (Lam "f" (Var "f"))
  -- An application: true false  (i.e. (λt.λf.t) (λt.λf.f))
  let app   = App true false
  let appM  = App true false
  let appN  = App false true
  let appP  = App true true
  let app1  = App appM appN
  let app2  = App app1 appP

  putStrLn "Pretty-printed terms:"
  putStrLn $ "true  = " ++ pretty true
  putStrLn $ "false = " ++ pretty false
  putStrLn $ "app   = " ++ pretty app
  putStrLn $ "test  = " ++ pretty app2

  putStrLn "\nRaw output:"
  print true
  print app
