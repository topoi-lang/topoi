{-# LANGUAGE LambdaCase #-}
module Topoi.Domain.Term where

import qualified Data.Text.Short as TS
import Data.HashMap.Strict

type Name = TS.ShortText

{-
    STLC declaration lies here
-}
data Term
    = Lam Name Term -- Abstraction
    | Var Name
    | App Term Term
    | Lit Lit
    | Let Name Term Term
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
    | TVar Name
    deriving (Ord, Eq)

instance Show Type where
    show = \case
        TInt -> "TInt"
        TBool -> "TBool"
        TFunc t1 t2 -> "TFunc :: " ++ show t1 ++ "->" ++ show t2
        TVar name -> "(TVar \"" ++ TS.toString name ++ "\")"

type Scope = HashMap Name Value
type Environment = HashMap Name Type
