module Syntax.Concrete where

-- This serves as the parse tree and contains
-- source location

import Data.Loc
import Data.Text (Text)

data Lit
  = Num Int
  | Str Text -- this is atom
  deriving (Show)

type Name = Text

type Type = Term

-- | Term from parsing
data Term
  = Var Name Loc -- x
  | Lam Name Term Loc -- \x. t
  | App Term Term Loc -- t u
  | Let Name Type Term Term Loc -- let x : A = t in u
  | Annotation Term Type Loc -- Type annotation
  | Universe Int Loc -- Type Type, aka universe
  deriving (Show)

data TBase = TInt | TBool | TChar deriving (Show, Eq)
