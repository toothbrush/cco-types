{-# LANGUAGE StandaloneDeriving,
             TypeSynonymInstances #-}
module CCO.Types where
    import CCO.SystemF.AG (Ty (..))

    import Debug.Trace
    
    type TyEnv = [(Var,Ty)]
    type TyVar = String
    type Var   = String
    
    data TySubst = Identity
                 | Sub TyVar Ty
                 | Dot TySubst TySubst
                 deriving Show

    deriving instance Show Ty

    class Substitutable a where
        applySubst :: TySubst -> a -> a

    instance Substitutable Ty where
        applySubst Identity t = t
        applySubst (Dot s1 s2) t = applySubst s1 (applySubst s2 t)
        applySubst (Sub a t0) (TyVar t) = if a == t
                                               then t0
                                               else TyVar t
        applySubst s@(Sub a t0) (Arr t1 t2) = Arr
                                                  (applySubst s t1)
                                                  (applySubst s t2)
        applySubst s (Forall tv ts) = trace "applySubst Forall" undefined

    instance Substitutable TyEnv where
        applySubst s [] = []
        applySubst s ((v,ts):r) = (v,applySubst s ts):applySubst s r
