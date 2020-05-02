{-# LANGUAGE OverloadedStrings #-}

module Syntax.Concrete where

-- This serves as the parse tree and contains
-- source location

import Data.Loc
import Data.Text (Text)

data Program = Program [Expr] Loc
  deriving (Show)

data Lit
  = Num Int
  | Str Text -- this is atom
  deriving (Show)

-- There is no operator in a s expression
data Ident = Ident Text Loc

-- Argument
data Argument = Argument Text Loc

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
  | VarDecl Ident Expr Loc -- Statement
  | LambdaDecl Argument Expr Loc

instance Located Expr where
  locOf (App _ _ loc) = loc
  locOf (Lit _ loc) = loc
  locOf (Var _ loc) = loc
  locOf (VarDecl _ _ loc) = loc
  locOf (LambdaDecl _ _ loc) = loc

instance Show Expr where
  show (App exp1 exp2 _) = "(App " <> show exp1 <> show exp2 <> ")\n"
  show (Lit expr _) = "(Lit " <> show expr <> ")"
  show (Var expr _) = "(Var " <> show expr <> ")"
  show (VarDecl ident expr _) = "(VarDecl " <> show ident <> " = " <> show expr <> ")\n"
  show (LambdaDecl arg expr _) = "(λ " <> show arg <> " . (" <> show expr <> "))\n"

instance Show Ident where
  show (Ident text _) = show text

instance Show Argument where
  show (Argument text _) = show text
