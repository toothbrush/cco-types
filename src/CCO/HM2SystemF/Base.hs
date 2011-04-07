-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM2SystemF.Base
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  Paul van der Walt <paul@denknerd.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- HM to System F gets called here. Implemented as UUAG.
--
-------------------------------------------------------------------------------

module CCO.HM2SystemF.Base (runAlgoW) where

import CCO.HM2SystemF.AG (
    algoW_Syn_HMTm, 
    wrap_HMTm, 
    sem_HMTm, 
    Inh_HMTm (..),
    gen,
    convertType
    )
import CCO.Types
import CCO.HM.Base (HMTm (..))
import CCO.SystemF.Base (SFTm (..), SFTy (..))
import Debug.Trace

------- TODO WARNING
-- DO Something more here, this isn't the last step!

runAlgoW :: HMTm -> SFTm
runAlgoW t = let (sf,ty,tysbst) = algoW_Syn_HMTm (wrap_HMTm (sem_HMTm t) inh_HMTm)
                 tyscheme       = gen (applySubst tysbst []) ty
                 finaltype      = turnIntoTypedTerm sf tyscheme
             in  trace (show tyscheme) finaltype

-- don't trust this:
turnIntoTypedTerm :: SFTm -> TyScheme -> SFTm
turnIntoTypedTerm tm (PlainTy t)    = SFTyApp tm (convertType t)
turnIntoTypedTerm tm (Forall tv ts) = SFTyLam tv (turnIntoTypedTerm tm ts) -- App tm (SFForall tv (turnIntoTypedTerm tm ts))

-- | The top-level inherited attribute to be passed to an attribute grammar
-- for System F. In our case, we want to start with an empty type 
-- environment.
inh_HMTm :: Inh_HMTm
inh_HMTm = Inh_HMTm { typeEnvironment_Inh_HMTm = []
                    , counter_Inh_HMTm = [0..]
                    }
