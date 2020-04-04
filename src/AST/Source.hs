module AST.Source where

-- import Data.Text (Text)

data TypedExpr 
  = TypedExpr {
      expr     :: Expr,
      exprType :: ExprType
    }

data ExprType 
  = AtomType
  | FunctionType {
      argType    :: ExprType,
      returnType :: ExprType
    }
  | TypeType {
      universe :: Int
    }
  | ConstructedType {
      name :: String,
      typeArgs :: [ExprType]
    }

  

data Expr 
  = Atom String
  | Variable String
  | FunctionApplication {
      function :: TypedExpr,
      argument :: TypedExpr
    }
