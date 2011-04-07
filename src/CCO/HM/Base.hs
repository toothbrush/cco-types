{-# LANGUAGE TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Base
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module CCO.HM.Base (
    -- * Syntax
    Var                         -- = String
  , HMTm (HMTm)                     -- instances: Tree
  , HMTm_ (Var, Lam, App, Let)    -- instances: Tree
) where

import CCO.HM.AG
import CCO.Tree                   (Tree (fromTree, toTree))
import CCO.Types
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))

import Debug.Trace

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree HMTm where
  fromTree (HMTm pos t) = T.App "HMTm" [fromTree pos, fromTree t]
  toTree = parseTree [app "HMTm" (HMTm <$> arg <*> arg)]

instance Tree HMTm_ where
  fromTree (Var x)       = T.App "Var" [fromTree x]
  fromTree (Lam x t1)    = T.App "Lam" [fromTree x, fromTree t1]
  fromTree (App t1 t2)   = T.App "App" [fromTree t1, fromTree t2]
  fromTree (Let x t1 t2) = T.App "Let" [fromTree x, fromTree t1, fromTree t2]

  toTree = parseTree [ app "Var" (Var <$> arg                )
                     , app "Lam" (Lam <$> arg <*> arg        )
                     , app "App" (App <$> arg <*> arg        )
                     , app "Let" (Let <$> arg <*> arg <*> arg)
                     ]

instance Eq HMTm_ where
    (==) (Var x) (Var y) = x == y
    (==) _       _       = trace "Eq HMTm_" undefined

instance Substitutable Ty where
    applySubst Identity    t = t
    applySubst (Dot s1 s2) t = applySubst s1 (applySubst s2 t)
    applySubst (Sub a  t0) (Alpha t)     = if a == t
                                           then t0
                                           else Alpha t
    applySubst s@(Sub a  t0) (Arrow t1 t2) = Arrow
                                              (applySubst s t1)
                                              (applySubst s t2)

instance Substitutable TyScheme where
    applySubst s (PlainTy t)    = PlainTy (applySubst s t)
    applySubst s (Forall tv ts) = trace "applySubst TyScheme" undefined

instance Substitutable TyEnv where
    applySubst s []         = []
    applySubst s ((v,ts):r) = (v,applySubst s ts):applySubst s r
