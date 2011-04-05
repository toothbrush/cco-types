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

module CCO.HM2SystemF.Base where

import CCO.HM2SystemF.AG

-- instance Printable Tm where
--   pp t = ppML_Syn_Tm (wrap_Tm (sem_Tm t) inh_Tm)
-- 
-- -- | The top-level inherited attributes to be passed to an attribute grammar
-- -- for System F.
-- inh_Tm :: Inh_Tm
-- inh_Tm = Inh_Tm {prec_Inh_Tm = 0}
