module Type.Infer where

import Data.IntMap (IntMap) -- NOTE: gonna substiture things
import Syntax.Abstract
import qualified Syntax.Concrete as Concrete

inferLit :: Concrete.Lit -> Type
inferLit (Concrete.Num _) = TPrimitive TInt
inferLit (Concrete.Str _) = TPrimitive TStr

inferExpr :: Concrete.Expr -> Type
inferExpr = undefined
