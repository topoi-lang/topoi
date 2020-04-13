module Syntax.Concrete where

-- This serves as the parse tree and contains
-- source location

import Data.Loc
import Data.Text (Text)

data Program = Program [Expr] Loc

data Lit
  = Num Int
  | Str Text -- this is atom
  deriving (Show)

-- There is no operator in a s expression
data Ident = Ident Text Loc deriving (Show)

data Type
  = TFunc Type Type Loc
  | TVar Ident Loc -- "some user defined type"
  | TList Type Loc
  | TUniverse Int Loc
  deriving (Show)

data Expr
  = App Expr Expr Loc -- Function application
  | Lit Lit Loc
  | Var Ident Loc
  deriving (Show)

instance Located Expr where
  locOf (App _ _ loc) = loc
  locOf (Lit _ loc) = loc
  locOf (Var _ loc) = loc
