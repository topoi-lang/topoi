{-# LANGUAGE OverloadedStrings #-}

module Syntax.Concrete where

-- This serves as the parse tree and contains
-- source location

import Data.Loc
import Data.Text (Text)

data Program = Program [Statement] Loc
  deriving (Show)

data Lit
  = Num Int
  | Str Text -- this is atom
  deriving (Show)

data UpperIdent = Upper Text Loc

data LowerIdent = Lower Text Loc

-- Does not care it is uppercase or not
data Ident = Ident Text Loc

data TBase
  = TInt
  | TBool
  | TChar
  deriving (Show, Eq)

data Type
  = TFunc Type Type Loc
  | TVar UpperIdent Loc -- "some user defined type"
  | TList Type Loc
  | TUniverse Int Loc
  | TBase TBase Loc
  deriving (Show)

data Expr
  = App Expr Expr Loc -- Function application
  | Lit Lit Loc
  | Var LowerIdent Loc
  | VarDecl Ident Expr Loc -- Statement

data Statement
  = TypeDecl LowerIdent Type Loc
  deriving (Show)

instance Located Expr where
  locOf (App _ _ loc) = loc
  locOf (Lit _ loc) = loc
  locOf (Var _ loc) = loc
  locOf (VarDecl _ _ loc) = loc

instance Located Type where
  locOf (TFunc _ _ loc) = loc
  locOf (TVar _ loc) = loc
  locOf (TList _ loc) = loc
  locOf (TUniverse _ loc) = loc
  locOf (TBase _ loc) = loc

instance Show Expr where
  show (App exp1 exp2 _) = "(App " <> show exp1 <> show exp2 <> ")"
  show (Lit expr _) = "(Lit " <> show expr <> ")"
  show (Var expr _) = "(Var " <> show expr <> ")"
  show (VarDecl ident expr _) = "(VarDecl " <> show ident <> " = " <> show expr <> ")"

instance Show Ident where
  show (Ident text _) = show text

instance Show LowerIdent where
  show (Lower text _) = show text

instance Show UpperIdent where
  show (Upper text _) = show text
