module CCO.Types (
    TyVar,
    Var,
    TyEnv,
    TyScheme (..),
    Ty (..),
    TySubst (..),
    applySubst,
    Substitutable
    ) where

    type TyVar    = String
    type Var      = String

    type TyEnv    = [(Var,TyScheme)]

    data TyScheme = PlainTy Ty
                  | Forall TyVar TyScheme
                  deriving Show

    data Ty       = Alpha TyVar
                  | Arrow Ty Ty
                  deriving Show

    data TySubst  = Identity
                  | Sub TyVar Ty
                  | Dot TySubst TySubst
                  deriving Show

    class Substitutable a where
        applySubst :: TySubst -> a -> a

