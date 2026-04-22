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
pretty (App e1 e2) = parenL e1 ++ " " ++ parenR e2
  where
    -- Parenthesize the left side if it's an application
    -- (to preserve left-associativity when printed)
    parenL e@(App _ _) = "(" ++ pretty e ++ ")"
    parenL e           = pretty e

    -- Parenthesize the right side if it's a lambda or application
    parenR e@(Lam _ _) = "(" ++ pretty e ++ ")"
    parenR e@(App _ _) = "(" ++ pretty e ++ ")"
    parenR e           = pretty e

main :: IO ()
main = do
  -- Church-encoded true: λt.λf.t
  let true  = Lam "t" (Lam "f" (Var "t"))
  -- Church-encoded false: λt.λf.f
  let false = Lam "t" (Lam "f" (Var "f"))
  -- An application: true false  (i.e. (λt.λf.t) (λt.λf.f))
  let app   = App true false

  putStrLn "Pretty-printed terms:"
  putStrLn $ "true  = " ++ pretty true
  putStrLn $ "false = " ++ pretty false
  putStrLn $ "app   = " ++ pretty app

  putStrLn "\nRaw output:"
  print true
  print app
