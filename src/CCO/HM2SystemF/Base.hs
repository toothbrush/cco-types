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

-- instance Printable Tm where
runAlgoW t = algoW_Syn_HMTm (wrap_HMTm (sem_HMTm t) inh_HMTm)
-- 
-- -- | The top-level inherited attributes to be passed to an attribute grammar
-- -- for System F.
inh_HMTm :: Inh_HMTm
inh_HMTm = Inh_HMTm --{prec_Inh_Tm = 0}
