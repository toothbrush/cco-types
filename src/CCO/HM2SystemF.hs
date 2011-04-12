{-# LANGUAGE StandaloneDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM2SystemF
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  Paul van der Walt <paul@denknerd.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- HM type inferencer. This function returns a type-annotated System F term,
-- given an implicitly-typed HM term. Calls the AG defined in Infer.ag. 
--
-------------------------------------------------------------------------------

module CCO.HM2SystemF (
    doConversion
) where

import CCO.HM.AG (
    Tm (..), Tm_ (..),
    inferredType_Syn_Tm,
    wrap_Tm,
    sem_Tm,
    substitution_Syn_Tm,
    Inh_Tm (..),
    annotated_Syn_Tm,
    gen
    )
import qualified CCO.SystemF.AG as SF (Tm (..))
import CCO.Types

import Debug.Trace

deriving instance Show Tm
deriving instance Show Tm_

doConversion :: Tm -> SF.Tm
doConversion t = let --noLet = withoutLet_Syn_Tm (wrap_Tm (sem_Tm t) inh_Tm)
                     inferredType = inferredType_Syn_Tm (wrap_Tm (sem_Tm t) inh_Tm)
                     substitution = substitution_Syn_Tm (wrap_Tm (sem_Tm t) inh_Tm)
                     annotated    = annotated_Syn_Tm (wrap_Tm (sem_Tm t) inh_Tm)
                     (ty', coercion) = gen [] inferredType
                 in 
                    --trace (show (inferredType,substitution) 
                    --        ++ "\nwithout Let: " ++ (show $ noLet)
                    --        ) 
                            trace (show ty') (coercion annotated)

-- | The top-level inherited attribute to be passed to an attribute grammar
-- for System F. In our case, we want to start with an empty type
-- environment, and a variable counter of 0 (used for generating fresh
-- type variables).
inh_Tm :: Inh_Tm
inh_Tm = Inh_Tm { typeEnvironment_Inh_Tm = []
                , counter_Inh_Tm = 0
                }
