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

import CCO.HM.AG (Tm)
import qualified CCO.SystemF.AG as SF (Tm (..))

doConversion :: Tm -> SF.Tm
doConversion = const $ SF.Var "s"

