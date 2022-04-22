-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Resources
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoning rules using the resource ontology.
--
-----------------------------------------------------------------------------

module ArM.Rules.Resource where

import Swish.RDF.Ruleset
import qualified Data.Text as T
import Swish.Rule
import Swish.RDF.Graph
import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Vocabulary.XSD
import ArM.Resources
import ArM.Rules.Aux
import Swish.VarBinding (varBindingId) 

prepareResources = fwdApplyList 
  [ vfpRule, vfpMajorRule, vfabRule, vfvRule, vffRule ]

vfpRule = makeCRule "vfpRule" [vfp1, vfp2, vfp3] [vfpT]
vfpMajorRule = makeCRule "vfpmajorRule" [vfp1, vfp2bis, vfp3] [vfpTbis]
vfp1 = arc sVar gtRes tVar
vfp2 = arc sVar typeRes (Res $ makeSN "minorFlaw")
vfp2bis = arc sVar typeRes (Res $ makeSN "majorFlaw")
vfp3 = arc tVar typeRes (Res $ makeSN "PersonalityTrait" )
vfpT = arc tVar (Res $ makeSN "hasScore") (litInt 3)
vfpTbis = arc tVar (Res $ makeSN "hasScore") (litInt 6)

litInt i = TypedLit (T.pack $ show i) xsdInt

vfabRule = makeCRule "vfabRule" [vfp1, vfab3] [vfabT]
vfab3 = arc tVar typeRes (Res $ makeSN "Ability" )
vfabT = arc tVar (Res $ makeSN "hasTotalXP") (litInt 5)

vfvRule = makeCRule "vfvRule" [vfp1, vfv3] [vfT]
vffRule = makeCRule "vffRule" [vfp1, vff3] [vfT]
vfv3 = arc tVar typeRes (Res $ makeSN "Virtue" )
vff3 = arc tVar typeRes (Res $ makeSN "Flaw" )
vfT = arc tVar (Res $ makeSN "hasTotalXP") (litInt 0)
