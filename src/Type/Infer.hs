{-# LANGUAGE OverloadedStrings #-}

module Type.Infer where

import Control.Monad.Except
import Control.Monad.State
import Data.HashMap.Strict
import Data.Loc
import Syntax.Abstract
import Syntax.Concrete hiding (Expr (..), Type (..))
import qualified Syntax.Concrete as Concrete
import Type.Error
import Prelude hiding (lookup)

inferLit :: Concrete.Lit -> Type
inferLit (Concrete.Num _) = TPrimitive TInt
inferLit (Concrete.Str _) = TPrimitive TStr

type SymbolTable = HashMap TVar Type

type SubstituteM = ExceptT TypeError (State SymbolTable)

-- Todo: make it emptySymbolTable, make function declaration
predefinedSymbolTable :: SymbolTable
predefinedSymbolTable =
  fromList
    [ ("succ", TFunc (TPrimitive TInt) (TPrimitive TInt)),
      ("not", TFunc (TPrimitive TBool) (TPrimitive TBool)),
      ("eqStr", TFunc (TPrimitive TStr) (TFunc (TPrimitive TStr) (TPrimitive TBool)))
    ]

runSubstituteM :: SubstituteM a -> Either TypeError a
runSubstituteM m = evalState (runExceptT m) predefinedSymbolTable

exceptM :: Monad m => Maybe a -> e -> (a -> ExceptT e m b) -> ExceptT e m b
exceptM (Just x) _ f = f x
exceptM Nothing e _ = throwError e

-- program :: Concrete.Expr
-- program =
--   Concrete.App
--     -- (Concrete.Lit (Concrete.Num 3) NoLoc)
--     -- (Concrete.Var (Concrete.Ident "succ" NoLoc) NoLoc)
--     (Concrete.Var (Concrete.Ident "not" NoLoc) NoLoc)
--     (Concrete.Lit (Concrete.Num 3) NoLoc)
--     NoLoc

inferProgram :: [Concrete.Expr] -> SubstituteM [Type]
inferProgram exprs = mapM inferExpr exprs

inferExpr :: Concrete.Expr -> SubstituteM Type
inferExpr (Concrete.Var (Ident x loc) _) = do
  symbolTable <- get
  exceptM (lookup x symbolTable) (Failed x loc) return
inferExpr (Concrete.Lit x _) = return (inferLit x)
inferExpr (Concrete.App expr1 expr2 loc) = do
  fnType <- inferExpr expr1
  case fnType of
    TFunc parameter returnType -> do
      t2 <- inferExpr expr2
      void $ unify loc parameter t2
      return returnType
    _ -> throwError (NotAFunction fnType loc)

unify :: Loc -> Type -> Type -> SubstituteM Type
unify loc (TPrimitive t1) (TPrimitive t2)
  | t1 == t2 = return (TPrimitive t1)
  | otherwise = throwError $ UnifyFailed (TPrimitive t1) (TPrimitive t2) loc
unify loc (TFunc t1 t2) (TFunc t3 t4) = do
  parameter1 <- unify loc t1 t3
  parameter2 <- unify loc t2 t4
  return (TFunc parameter1 parameter2)
unify loc t1 t2 = throwError (UnifyFailed t1 t2 loc) -- otherwise
