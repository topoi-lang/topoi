module Syntax.Concrete where

-- This serves as the parse tree and contains
-- source location in this case
import Data.Loc
import Data.Text (Text)
import qualified Data.Text as Text

data Program = Program [Expr] Loc

data Lit
  = Num Int
  | -- | Bol Bool
    Str Text
  deriving (Show)

data VarName = VarName Text Loc deriving (Show)

data Type
  = TFunc Type Type Loc
  | TVar VarName Loc -- "some user defined type"
  | TList Type Loc -- "'(...)"
  deriving (Show)

data Expr
  = App Expr Expr Loc -- Function application
  | Lit Lit Loc
  | Var VarName Loc

-- testing purpose
instance Show Expr where
  show expr = case expr of
    App _ _ _ -> "App"
    Lit (Num i) _ -> "Lit " <> show i
    Lit (Str s) _ -> "Lit " <> Text.unpack s
    Var (VarName s _) _ -> "Var " <> Text.unpack s
