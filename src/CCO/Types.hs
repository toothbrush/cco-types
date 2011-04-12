{-# LANGUAGE StandaloneDeriving,
             TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Types
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  Paul van der Walt <paul@denknerd.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module contains some types which are useful for Algorithm W. 
-- A number of helper functions are also defined here. 
--
-------------------------------------------------------------------------------
module CCO.Types where
    import CCO.SystemF.AG (Ty (..), Tm (..))
    import Data.List

    import Debug.Trace
    
    type TyEnv = [(Var,Ty)]
    type TyVar = String
    type Var   = String
    
    -- | A type substitution. Used for substitution of type variables
    -- in signatures. 
    data TySubst = Identity -- ^ Identity substitution, do nothing. 
                 | Sub TyVar Ty -- ^ Substitute a type variable with a type. 
                 | Dot TySubst TySubst -- ^ Chain substitutions together.
                 deriving Show

    instance Show Ty where 
        show (TyVar x) = x
        show (Arr t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
        show (Forall tv ty) = "(forall " ++ tv ++ ". " ++ show ty ++ ")"

    deriving instance Show Tm

    -- | A class which is useful for defining functions such as 'applySubst' (which
    -- substitutes all occurences of a variable with a type, and 'ftv' which returns
    -- all the free type variables in the argument.
    class Types a where
        applySubst :: TySubst -> a -> a
        ftv        :: a -> [Var]

    instance Types Ty where
        ftv (TyVar tv) = [tv]
        ftv (Arr t1 t2) = nub(ftv t1 ++ ftv t2)
        ftv (Forall tv ts) = nub (ftv ts) \\ [tv]
        applySubst Identity t = t
        applySubst (Dot s1 s2) t = applySubst s1 (applySubst s2 t)
        applySubst (Sub a t0) (TyVar t) = if a == t
                                               then t0
                                               else TyVar t
        applySubst s@(Sub a t0) (Arr t1 t2) = Arr
                                                  (applySubst s t1)
                                                  (applySubst s t2)
        applySubst s@(Sub a t0) (Forall tv ts) = if a == tv
                                                    then trace "hmmmm" (Forall tv t0)
                                                    else Forall tv (applySubst s ts)

    instance Types TyEnv where
        applySubst s [] = []
        applySubst s ((v,ts):r) = (v,applySubst s ts):applySubst s r
        ftv [] = []
        ftv ((v,ts):r) = ftv ts ++ ftv r

    -- | The unification algorithm. If none of the cases match, fail.
    unify :: Ty -> Ty -> TySubst
    unify t1@(TyVar tv1) t2@(TyVar tv2) | tv1 == tv2 = Identity
                                        | not (elem tv1 (ftv t2)) = Sub tv1 t2
                                        | not (elem tv2 (ftv t1)) = Sub tv2 t1
                                        | otherwise = error "Cannot unify. Error."
    unify (TyVar tv1) t | not (elem tv1 (ftv t)) = Sub tv1 t
                        | otherwise = error $ "occurs check: " ++
                                                          show tv1 ++ ", " ++
                                                          show t
    unify t (TyVar tv2) | not (elem tv2 (ftv t)) = Sub tv2 t
                        | otherwise = error $ "occurs check: " ++ 
                                                          show tv2 ++ ", " ++
                                                          show t
    unify (Arr t11 t12) (Arr t21 t22) = let theta1 = unify t11 t21
                                            theta2 = unify
                                                        (applySubst theta1 t12)
                                                        (applySubst theta1 t22)
                                        in Dot theta2 theta1
    unify t1 t2 = error$"Unification failure. \nt_1 = " ++ show t1 ++ "\nt_2 = " ++ show t2
