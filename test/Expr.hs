module Expr where

data Expr = ELambda [Expr] Expr
          | EApp    Expr [Expr]
          | EId     String
          | EInt    Int
          deriving (Eq, Show)
