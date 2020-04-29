module Syntax.Abstract where

import Data.Text (Text)

type Universe = Int

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
  deriving (Show)

data TPrimitive
  = TInt
  | TBool
  | TStr {- should be TChr, but atom is a text so... -}
  deriving (Show, Eq)

data Type
  = TFunc Type Type
  | TPrimitive TPrimitive
  -- TVar TVar Type
  deriving (Show)
