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

-- | Infere resource properties from class
prepareGraph = fwdApplyList [ advancevfgrantRule, grantRule, spectraitRule, rRule ]

rRule = makeCRule "rRule" l1 l2
    where l1 = [ arc sVar ( Res $ makeSN "traitClass" ) tVar,
               arc tVar pVar oVar,
               arc pVar typeRes ( Res $ makeSN "TraitProperty" )  ]
          l2 = [arc sVar pVar oVar]


stcRes = Res $ makeSN "SpecialTraitClass" 
stcArc = arc tVar typeRes stcRes
isSTRes = Res $ makeSN "isSpecialTrait" 
isSTArc = arc sVar isSTRes tVar

spectraitRule = makeCRule  "spectraitRule" [ tArc, stcArc ] [ isSTArc ]

atRes = Res $ makeSN "advanceTrait" 
gtRes = Res $ makeSN "grantsTrait" 
htRes = Res $ makeSN "hasTrait" 
csRes = Res $ makeSN "CharacterSheet" 
caRes = Res $ makeSN "CharacterAdvancement" 

-- | apply grantsTrait to a CharacterSheet
grantRule = makeCRule  "grantRule" 
     [ arc sVar htRes oVar,
       arc sVar typeRes csRes,
       arc oVar gtRes cVar ]
     [ arc sVar htRes cVar ]
-- | apply grantsTrait to an Advancement
advancevfgrantRule = makeCRule  "advancevfgrantRule" 
     [ arc cVar gtRes oVar,
       arc sVar typeRes caRes,
       arc sVar atRes cVar ]
     [ arc sVar atRes oVar ]

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
