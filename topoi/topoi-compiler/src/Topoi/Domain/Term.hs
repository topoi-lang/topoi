module Topoi.Domain.Term where

import qualified Data.Text.Short as TS
import Data.HashMap.Strict

type Name = TS.ShortText

{-
    STLC declaration lies here
-}
data Term
    = Lam Name Term
    | Var Name
    | App Term Term
    | Lit Lit
    deriving (Show)

data Lit
    = LInt Int
    | LBool Bool
    deriving (Show)

data Value
    = VInt Int
    | VBool Bool
    | VClosure Name Term Scope

data Type
    = TInt
    | TBool
    | TFunc Type Type

type Scope = HashMap Name Value
type Environment = HashMap Name Type
