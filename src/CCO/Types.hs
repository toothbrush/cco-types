{-# LANGUAGE StandaloneDeriving #-}
module CCO.Types where
    import CCO.SystemF.AG (Ty (..))
    
    type TyEnv = [(Var,Ty)]
    type TyVar = String
    type Var   = String
    
    data TySubst = Identity
                 | Sub TyVar Ty
                 | Dot TySubst TySubst
                 deriving Show

    deriving instance Show Ty
