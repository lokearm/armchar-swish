-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Resources
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Rules to prepare the resource graph.
--
-- Only one function is included.
--
-- 1.  Traits are created as inferred by virtues and flaws.
--
-----------------------------------------------------------------------------

module ArM.Rules.Resource (prepareResources) where

import qualified Data.Text as T
import Swish.RDF.Graph
import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Vocabulary.XSD
import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.Common
import ArM.Rules.RDFS

-- | Prepare the resource graph.
-- This is applied without any schema or other data but the resources.
prepareResources = fwdApplyList ( vfabRule:personalityflawRules ) 
                 . fwdApplyListR vfRules
                 . applyRDFS
                 . fwdApplyList [ traitclasstypeRule ]

traitclass = Var "traitclass"
trait = Var "trait"
score = armRes "hasScore"


-- | The rules to derive personality traits from personality flaws.
personalityflawRules = 
  [ makeCRule "minor-personality-flaw" [grantarc
     , arc traitclass subclassRes (armRes "minorFlaw")
     , vfp3
     ] [ arc trait score (litInt 3) ]
  , makeCRule "major-personality-flaw" [grantarc
     , arc traitclass subclassRes (armRes "majorFlaw")
     , vfp3
     ] [ arc trait score (litInt 6) ]
  ]
  where vfp3 = arc trait typeRes (armRes "PersonalityTrait" )

-- | Auxiliary arc used several times
grantarc = arc traitclass gtRes trait

-- | Rule to infer abilities granted by virtues and flaws
vfabRule = makeCRule "abilities-from-virtues" [grantarc, vfab3] [vfabT]
vfab3 = arc trait typeRes (armRes "Ability" )
vfabT = arc trait (armRes "hasTotalXP") (litInt 5)

-- | Rules to infer virtues/flaws granted by virtues and flaws
vfRules =
   [ makeCRule "vfvRule" [grantarc, vfv3] [vfT]
   , makeCRule "vffRule" [grantarc, vff3] [vfT]
   ] where
       vfv3 = arc trait typeRes (armRes "Virtue" )
       vff3 = arc trait typeRes (armRes "Flaw" )
       vfT = arc trait score (litInt 0)
