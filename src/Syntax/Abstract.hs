module Syntax.Abstract where

import Data.Text (Text)

type Universe = Int

-- identical to Concrete.Ident
type Var = Text

type TVar = Text

data Lit
  = Num Int
  | Chr Char
  deriving (Show)

data Expr
  = Var Var
  | Lit Lit
  | App Expr Expr
  | VarDecl Var Expr
  deriving (Show)

data TPrimitive
  = TInt
  | TBool
  | TStr {- should be TChr, but atom is a text so... -}
  deriving (Show, Eq)

data Type
  = TFunc Type Type
  | TPrimitive TPrimitive
  | TVoid -- bottom type for statement
  deriving (Show)
