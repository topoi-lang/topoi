{-# LANGUAGE UndecidableInstances #-}

module Topoi.TypeCheck where

import Topoi.Domain.Term
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Text.Short as TS

data Scheme = Scheme [Name] Type

type Substitution = Map.HashMap Name Type

class Types a where
    -- free type variable
    ftv :: a -> Set.HashSet Name
    -- apply substitution
    applySubst :: Substitution -> a -> a

instance Types Type where
    ftv (TVar name) = Set.singleton name
    ftv TInt = Set.empty
    ftv TBool = Set.empty
    ftv (TFunc t1 t2) = ftv t1 `Set.union` ftv t2


    -- Basically lookup the name from the `Substitution` map, returns the value
    applySubst s (TVar name) = case Map.lookup name s of
        Nothing -> TVar name
        Just t -> t

    applySubst s (TFunc t1 t2) = TFunc (applySubst s t1) (applySubst s t2)
    
    applySubst s t = t

instance Types Scheme where
    ftv (Scheme vars t) = (ftv t) `Set.difference` (Set.fromList vars)
    applySubst s (Scheme vars t) = Scheme vars (applySubst (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    applySubst s = fmap (applySubst s)
    ftv       xs = foldr (Set.union . ftv) Set.empty xs

noSubst :: Substitution
noSubst = Map.empty

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.union (Map.map (applySubst s1) s2) s1

newtype TypeEnv = TypeEnv (Map.HashMap Name Scheme)

-- remove the binding of types-- remove the binding of types
remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) varName = TypeEnv (Map.delete varName env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    applySubst s (TypeEnv env) = TypeEnv (Map.map (applySubst s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

-- fresh name for newly introduces type variables
data TIEnv = TIEnv {}
type TIState = Int

type TI a = ExceptT Name (State TIState) a

runTI :: TI a -> (Either Name a, TIState)
runTI t = runState (runExceptT t) initTIState where initTIState = 0

newTyVar :: TI Type
newTyVar = do
    s <- get
    put (s + 1)
    return (TVar $ TS.fromString (reverse (toTyVar s)))
  where
    toTyVar :: Int -> String
    toTyVar c | c < 26 = [toEnum (97 + c)]
              | otherwise = let (n, r) = c `divMod` 26 in (toEnum (97 + r)) : toTyVar (n - 1)

-- replaces all bound type variables in a type scheme with fresh type variables
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (const newTyVar) vars
    let s = Map.fromList (zip vars nvars)
    return $ applySubst s t

-- algorithm U
-- unification function for types
mgu :: Type -> Type -> TI Substitution
mgu (TFunc l r) (TFunc l' r')  = do
    s1 <- mgu l l'
    s2 <- mgu (applySubst s1 r) (applySubst s1 r')
    return (s1 `composeSubst` s2)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt TInt = return noSubst
mgu TBool TBool = return noSubst
mgu t1 t2 = throwError $ "types do not unify: "-- <> show t1 -- ++ show t1 ++ " and " ++ show t2

varBind :: Name -> Type -> TI Substitution
varBind u t | t == TVar u = return noSubst
            | u `Set.member` ftv t = throwError $ "occur check fails: " -- ++ " " -- ++ TS.toString u ++ " and " ++ show t
            | otherwise = return (Map.singleton u t)

typeLit :: TypeEnv -> Lit -> TI (Substitution, Type)
typeLit _ (LInt _) = return (noSubst, TInt)
typeLit _ (LBool _) = return (noSubst, TBool)

ti :: TypeEnv -> Term -> TI (Substitution, Type)
ti (TypeEnv env) (Var name) = case Map.lookup name env of
    Nothing -> throwError $ "unbound variable: " -- ++ show name
    Just sigma -> do
        t <- instantiate sigma
        return (noSubst, t)
ti env (Lit lit) = typeLit env lit
ti env (Lam name term) = do
    tv <- newTyVar
    let TypeEnv env' = remove env name
    let env'' = TypeEnv (env' `Map.union` (Map.singleton name (Scheme [] tv)))
    (s1, t1) <- ti env'' term
    return (s1, TFunc (applySubst s1 tv) t1)
ti env (App term1 term2) = do
    tv <- newTyVar
    (s1, t1) <- ti env term1
    (s2, t2) <- ti (applySubst s1 env) term2
    s3 <- mgu (applySubst s2 t1) (TFunc t2 tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, applySubst s3 tv)
ti env (Let name term1 term2) = do
    (s1, t1) <- ti env term1
    let TypeEnv env' = remove env name
    let t' = generalize (applySubst s1 env) t1
    let env'' = TypeEnv (Map.insert name t' env')
    (s2, t2) <- ti (applySubst s1 env'') term2
    return (s1 `composeSubst` s2, t2)

typeInference :: TypeEnv -> Term -> TI Type
typeInference (TypeEnv env) term = do
    (s, t) <- ti (TypeEnv env) term
    return (applySubst s t)


aaa :: String
aaa = show (TVar $ TS.fromString "aaa")