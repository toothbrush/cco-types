module CCO.Types (TyVar,Var,TyEnv,TyScheme (..),Ty (..),TySubst (..),applySubst,Substitutable) where

    type TyVar    = String
    type Var      = String

    type TyEnv    = [(Var,TyScheme)]

    data TyScheme = Ty
                  | Forall TyVar TyScheme

    data Ty       = Alpha TyVar
                  | Arrow Ty Ty

    data TySubst  = Identity
                  | Sub TyVar Ty
                  | Dot TySubst TySubst

    class Substitutable a where
        applySubst :: TySubst -> a -> a

