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

import CCO.HM2SystemF.AG (algoW_Syn_HMTm, wrap_HMTm, sem_HMTm, Inh_HMTm (..))
import CCO.Types
import CCO.HM.Base (HMTm (..))
import CCO.SystemF.Base (SFTm (..))

runAlgoW :: HMTm -> SFTm
------- TODO WARNING
-- DO Something more here, this isn't the last step!
runAlgoW t = let (sf,ty,tysbst) = algoW_Syn_HMTm (wrap_HMTm (sem_HMTm t) inh_HMTm)
             in sf
-- | The top-level inherited attribute to be passed to an attribute grammar
-- for System F. In our case, we want to start with an empty type 
-- environment.
inh_HMTm :: Inh_HMTm
inh_HMTm = Inh_HMTm { typeEnvironment_Inh_HMTm = []
                    , counter_Inh_HMTm = 0
                    }
