module Hutton'sRazor where

-- | Exercise 1
data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e e') = eval e + eval e'

-- | Exercise 2
printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e e') = printExpr e ++ " + " ++ printExpr e'
