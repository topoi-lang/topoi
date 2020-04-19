module Type.TypedExpr where

import qualified Syntax.Concrete as Concrete
import Type.Type

data TypedExpr = TypedExpr Concrete.Expr Type