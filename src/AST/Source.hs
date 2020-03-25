module AST.Source (SExpr (..)) where

data SExpr
  = Atom String -- We treat atom as string
  | Int Int
  | Float Double
  | Op String
  | Unit
