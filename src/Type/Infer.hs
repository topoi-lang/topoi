module Type.Infer where

import Data.IntMap (IntMap) -- NOTE: gonna substiture things
import Data.Loc
import Syntax.Abstract
import Type.TypedExpr
import Type.SymbolTable
import qualified Syntax.Concrete as Concrete

inferLit :: Concrete.Lit -> Type
inferLit (Concrete.Num _) = TPrimitive TInt
inferLit (Concrete.Str _) = TPrimitive TStr

data Error = Error {
  loc :: Loc,
  message :: String
}

inferProgram :: [Concrete.Expr] -> Either (SymbolTable, [TypedExpr]) Error
inferProgram exprs = inferProgram' emptySymbolTable exprs

inferProgram' :: SymbolTable -> [Concrete.Expr] -> Either (SymbolTable, [TypedExpr]) Error
inferProgram' symbolTable exprs = 
  mapM 
    (\expr -> inferExpr expr)
    exprs


inferExpr :: SymbolTable -> Concrete.Expr -> Either (SymbolTable, TypedExpr) Error
inferExpr = undefined
