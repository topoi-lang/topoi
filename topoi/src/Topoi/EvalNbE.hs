module Topoi.EvalNbE where

import qualified Data.HashMap.Strict as Map
import qualified Data.Text.Short as TS
import Data.Maybe (fromJust)

{- |
[GluedEval.hs](https://gist.github.com/AndrasKovacs/a0e0938113b193d6b9c1c0620d853784) from
AndrasKovacs. A non-deterministic normalization-by-evaluation in Olle Fredriksson's flavor.
-}

type Name = TS.ShortText

{-|
We have a distinction between top and local scope, and we want to have control over unfolding of
top-level definitions.

- For generating terms (for error displaying, serialization, metavariable solutions), we want to
  avoid unfolding as much as possible, to avoid size blowups.

- For conversion checking, we want to have an efficient, preferably call-by-need evaluator which
  unfolds everything.
-}
data Term
    = Local !Name -- ^ local variable
    | Top !Name   -- ^ top-level variable
    | App !Term !Term
    | Let !Name !Term !Term
    | Lam !Name !Term
    deriving Show

data Spine = SNil | SApp !Spine Value

data Value
    = VLam !Name !(Value -> Value)
    | VLocal !Name !Spine
    | VTop !Name !Spine Value

type TopEnv = Map.HashMap Name Value
type LocalEnv = Map.HashMap Name (Maybe Value)

eval :: TopEnv -> LocalEnv -> Term -> Value
eval top local = \case
    Local x -> maybe (VLocal x SNil) id $ fromJust (Map.lookup x local)
    Top x -> VTop x SNil $ fromJust (Map.lookup x top)
    App t u -> vapp (eval top local t) (eval top local u)
    Let x t u -> eval top (Map.insert x (Just (eval top local t)) local) u
    Lam x t -> VLam x \u -> eval top (Map.insert x (Just u) local) t

vapp :: Value -> Value -> Value
vapp (VLam _ t) u = t u
vapp (VLocal x spine) u = VLocal x (SApp spine u)
vapp (VTop x spine t) u = VTop x (SApp spine u) (vapp t u)

fresh :: LocalEnv -> Name -> Name
fresh env x = case Map.lookup x env of
    Nothing -> x
    Just{} -> fresh env (x <> "'")

type UnfoldTop = Bool

quoteSpine :: LocalEnv -> UnfoldTop -> Term -> Spine -> Term
quoteSpine local unf h = \case
    SNil -> h
    SApp spine u -> App (quoteSpine local unf h spine) (quote local unf u)

quote :: LocalEnv -> UnfoldTop -> Value -> Term
quote local unf = \case
    VLocal x spine -> quoteSpine local unf (Local x) spine
    VLam x t ->
        let x' = fresh local x
        in Lam x' (quote (Map.insert x' Nothing local) unf (t (VLocal x' SNil)))
    VTop x spine t -> if unf then quote local unf t
                             else quoteSpine local unf (Top x) spine

evalTop :: Map.HashMap Name Term -> Term -> Value
evalTop top t = eval topvals Map.empty t
    where topvals = Map.foldlWithKey'
            (\top' x t' -> Map.insert x (eval top' Map.empty t') top')
            Map.empty
            top

nfTop :: UnfoldTop -> Map.HashMap Name Term -> Term -> Term
nfTop unf top t = quote Map.empty unf (evalTop top t)

($$) :: Term -> Term -> Term
($$) = App

infixl 8 $$

topDfn :: Map.HashMap Name Term
topDfn = Map.fromList [
    ("zero", Lam "s" $ Lam "z" $ Local "z")
  , ("suc",  Lam "n" $ Lam "s" $ Lam "z" $
        Local "s" $$ (Local "n" $$ Local "s" $$ Local "z"))
  , ("add",  Lam "a" $ Lam "b" $ Lam "s" $ Lam "z" $
        Local "a" $$ Local "s" $$ (Local "b" $$ Local "s" $$ Local "z"))
  , ("mul",  Lam "a" $ Lam "b" $ Lam "s" $ Lam "z" $
        Local "a" $$ (Local "b" $$ Local "s") $$ Local "s" $$ Local "z")
  , ("5", Top "suc" $$ (Top "suc" $$ (Top "suc" $$ (Top "suc" $$
           (Top "suc" $$ Top "zero")))))
  , ("10", Top "add" $$ Top "5" $$ Top "5")
  , ("100", Top "mul" $$ Top "10" $$ Top "10")
  ]

tm :: Term
tm = Let "foo" (Lam "s" $ Lam "z" $
                  Local "s" $$ (Local "s" $$ (Local "s" $$ Local "z"))) $
     Top "mul" $$ Local "foo" $$ Local "foo"

_test :: IO ()
_test = print $ nfTop False topDfn tm
