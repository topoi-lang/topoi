module Syntax.Concrete where

{- Note
~~~~~~~~~~~~

This serves as the parse tree and all the terms are
containing the source location for error reporting.
-}

import Data.Loc
import Data.Text (Text)

data Program
  = Program
      [Declaration]
      [Stmt] -- main program
      [Expr] -- variable on the root level, treat as global variable, I think
      Env
      Loc

data Name = Name Text Loc
  deriving (Show)

type Env = [(Text, Expr)]

data Declaration
  = VarDecl Name (Maybe Type) Expr Loc
  | TypeDecl Name Type Loc -- type annotation
  | LetDecl Name [Text] Expr Loc -- let <name> <args...> = <expr>
      -- I am trying to make something like let x : A = t in u,
      -- but just make things easier to be done, I choose to do in this way

data Stmt
  = Assign Name [Expr] Loc
  | Assert Expr Loc
  deriving (Show)

data Expr
  = Lit Lit Loc -- base typed variable
  | Var Name Loc -- x
  | Lam Name Expr Loc -- \x. t
  deriving (Show)

data Type
  = TBase TBase
  | TFunc Type Type
  | TVar Text
  deriving (Eq, Show)

data Lit = Num Int | Bol Bool | Chr Char
  deriving (Show, Eq)

data TBase = TInt | TBool | TChar
  deriving (Show, Eq)

tInt, tBool, tChar :: Type
tInt = TBase TInt
tBool = TBase TBool
tChar = TBase TChar
