-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.SystemF.Base
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

module CCO.SystemF.Base (
    -- * Syntax
    TyVar                               -- = String
  , Var                                 -- = String
  , SFTy (SFTyVar, SFArr, SFForall)             -- instances: Tree
  , SFTm (SFVar, SFLam, SFApp, SFTyLam, SFTyApp)    -- instances: Tree, Printable
) where

import CCO.SystemF.AG
import CCO.Printing               (Printable (pp))
import CCO.Tree                   (Tree (fromTree, toTree))
import CCO.Types
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree SFTy where
  fromTree (SFTyVar a)      = T.App "SFTyVar" [fromTree a]
  fromTree (SFArr ty1 ty2)  = T.App "SFArr" [fromTree ty1, fromTree ty2]
  fromTree (SFForall a ty1) = T.App "SFForall" [fromTree a, fromTree ty1]

  toTree = parseTree [ app "SFTyVar"  (SFTyVar  <$> arg        )
                     , app "SFArr"    (SFArr    <$> arg <*> arg)
                     , app "SFForall" (SFForall <$> arg <*> arg)
                     ]

instance Tree SFTm where
  fromTree (SFVar x)       = T.App "SFVar"   [fromTree x]
  fromTree (SFLam x ty t1) = T.App "SFLam"   [fromTree x, fromTree ty, fromTree t1]
  fromTree (SFApp t1 t2)   = T.App "SFApp"   [fromTree t1, fromTree t2]
  fromTree (SFTyLam a t1)  = T.App "SFTyLam" [fromTree a, fromTree t1]
  fromTree (SFTyApp t1 ty) = T.App "SFTyApp" [fromTree t1, fromTree ty]

  toTree = parseTree [ app "SFVar"   (SFVar <$> arg                )
                     , app "SFLam"   (SFLam <$> arg <*> arg <*> arg)
                     , app "SFApp"   (SFApp <$> arg <*> arg        )
                     , app "SFTyLam" (SFTyLam <$> arg <*> arg      )
                     , app "SFTyApp" (SFTyApp <$> arg <*> arg      )
                     ]

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

instance Printable SFTm where
  pp t = ppML_Syn_SFTm (wrap_SFTm (sem_SFTm t) inh_SFTm)

-------------------------------------------------------------------------------
-- Top-level inherited attributes
-------------------------------------------------------------------------------

-- | The top-level inherited attributes to be passed to an attribute grammar
-- for System F.
inh_SFTm :: Inh_SFTm
inh_SFTm = Inh_SFTm {prec_Inh_SFTm = 0}
