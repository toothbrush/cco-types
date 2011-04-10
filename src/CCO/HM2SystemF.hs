-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.SystemF
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  Paul van der Walt <paul@denknerd.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- System F.
--
-------------------------------------------------------------------------------

module CCO.HM2SystemF (
    doConversion
) where

import CCO.HM.AG (
    Tm,
    inferredType_Syn_Tm,
    wrap_Tm,
    sem_Tm,
    substitution_Syn_Tm,
    Inh_Tm (..),
    annotated_Syn_Tm,
    withoutLet_Syn_Tm 
    )
import qualified CCO.SystemF.AG as SF (Tm (..))
import CCO.Types

import Debug.Trace

doConversion :: Tm -> SF.Tm
doConversion t = let noLet = withoutLet_Syn_Tm (wrap_Tm (sem_Tm t) inh_Tm)
                     inferredType = inferredType_Syn_Tm (wrap_Tm (sem_Tm noLet) inh_Tm)
                     substitution = substitution_Syn_Tm (wrap_Tm (sem_Tm noLet) inh_Tm)
                     annotated    = annotated_Syn_Tm (wrap_Tm (sem_Tm noLet) inh_Tm)
                 in trace (show (inferredType,substitution) 
                          --  ++ "\nAnnotated: " ++ (show $ annotated)
                            ) 
                            annotated

-- | The top-level inherited attribute to be passed to an attribute grammar
-- for System F. In our case, we want to start with an empty type
-- environment.
inh_Tm :: Inh_Tm
inh_Tm = Inh_Tm { typeEnvironment_Inh_Tm = []
                , counter_Inh_Tm = 0
                }
