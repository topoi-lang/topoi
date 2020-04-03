module AST.Source where

-- import Data.Text (Text)

data Expr = Unary OP Expr Expr

data OP
  = OPCons
  | OPCar
  | OPCdr
  deriving (Eq)
