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
prepareResources = fwdApplyList [ vfpRule, vfpMajorRule, vfabRule ] 
                 . fwdApplyListR [ vfvRule, vffRule ]
                 . applyRDFS
                 . fwdApplyList [ traitclasstypeRule ]
traitclass = Var "traitclass"
trait = Var "trait"
score = armRes "hasScore"

grantarc = arc traitclass gtRes trait

vfpRule = makeCRule "vfpRule" [grantarc, vfp2, vfp3] [vfpT]
vfpMajorRule = makeCRule "vfpMajorRule" [grantarc, vfp2bis, vfp3] [vfpTbis]
vfp2 = arc traitclass subclassRes (armRes "minorFlaw")
vfp2bis = arc traitclass subclassRes (armRes "majorFlaw")
vfp3 = arc trait typeRes (armRes "PersonalityTrait" )
vfpT = arc trait score (litInt 3)
vfpTbis = arc trait score (litInt 6)


vfabRule = makeCRule "vfabRule" [grantarc, vfab3] [vfabT]
vfab3 = arc trait typeRes (armRes "Ability" )
vfabT = arc trait (armRes "hasTotalXP") (litInt 5)

vfvRule = makeCRule "vfvRule" [grantarc, vfv3] [vfT]
vffRule = makeCRule "vffRule" [grantarc, vff3] [vfT]
vfv3 = arc trait typeRes (armRes "Virtue" )
vff3 = arc trait typeRes (armRes "Flaw" )
vfT = arc trait score (litInt 0)
