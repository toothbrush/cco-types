{-# LANGUAGE StandaloneDeriving,
             TypeSynonymInstances #-}
module CCO.Types where
    import CCO.SystemF.AG (Ty (..))
    import Data.List

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
    
    class HasTypeVariables a where
        ftv :: a -> [Var]

    instance Substitutable Ty where
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

    instance Substitutable TyEnv where
        applySubst s [] = []
        applySubst s ((v,ts):r) = (v,applySubst s ts):applySubst s r

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
        
    instance HasTypeVariables Ty where
        ftv (TyVar tv) = [tv]
        ftv (Arr t1 t2) = nub(ftv t1 ++ ftv t2)
        ftv (Forall tv ts) = nub (ftv ts) \\ [tv]

    instance HasTypeVariables TyEnv where
        ftv [] = []
        ftv ((v,ts):r) = ftv ts ++ ftv r
