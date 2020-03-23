module AST.Source (Expr (..)) where

data Expr
  = Atom String -- We treat atom as string
  | Int Int
  | Float Double
  | Op String
  | Unit
