-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.SystemF
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- System F.
--
-------------------------------------------------------------------------------

module CCO.SystemF (
    -- * Syntax
    TyVar                               -- = String
  , Var                                 -- = String
  , SFTy (SFTyVar, SFArr, SFForall)             -- instances: Tree
  , SFTm (SFVar, SFLam, SFApp, SFTyLam, SFTyApp)    -- instances: Tree
) where

import CCO.SystemF.Base    (TyVar, Var, SFTy (..), SFTm (..))
