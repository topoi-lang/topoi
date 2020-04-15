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

data TPrimitive = TInt | TBool | TChar
  deriving (Show)

data Type
  = TFunc Type Type
  | TPrimitive TPrimitive
  | TVar TVar
  deriving (Show)
