module Type.SymbolTable where

import Data.HashMap.Strict
import Type.TypedExpr
import Syntax.Concrete

type VariableName = String

data Entry 
  = Typed TypedExpr  
  | Untyped Expr


type SymbolTable = HashMap VariableName Entry

emptySymbolTable :: SymbolTable
emptySymbolTable = empty