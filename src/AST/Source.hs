module AST.Source where

import Data.Text (Text)

data TypedExpr
  = TypedExpr
      { expr :: Expr,
        exprType :: ExprType
      }

data ExprType
  = AtomType
  | FunctionType
      { argType :: ExprType,
        returnType :: ExprType
      }
  | TypeType
      { universe :: Int
      }
  | ConstructedType
      { name :: Text,
        typeArgs :: [ExprType]
      }

data Expr
  = Unit
  | Atom Text
  | Number Int
  | Function Text [Expr]
  deriving (Show)
  -- | FunctionApplication
      -- { function :: TypedExpr,
        -- argument :: TypedExpr
      -- }
