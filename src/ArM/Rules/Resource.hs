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
import ArM.Resources
import ArM.Rules.Aux
import Swish.VarBinding (varBindingId) 
import Data.Set

-- | Infere resource properties from class
prepareGraph = fwdApplyList [ spectraitRule, rRule ]

rRule = makeCRule "rRule" l1 l2
    where l1 = [ arc sVar ( Res $ makeSN "traitClass" ) tVar,
               arc tVar pVar oVar,
               arc pVar typeRes ( Res $ makeSN "TraitProperty" )  ]
          l2 = [arc sVar pVar oVar]

listToRDFGraph :: [RDFTriple] -> RDFGraph
listToRDFGraph = toRDFGraph .  fromList 

stcRes = Res $ makeSN "SpecialTraitClass" 
stcArc = arc tVar typeRes stcRes
isSTRes = Res $ makeSN "isSpecialTrait" 
isSTArc = arc sVar isSTRes tVar

spectraitRule = makeCRule  "spectraitRule" 
           [ tArc, stcArc ] [ isSTArc ]
-- "?s rdf:type ?t . ?t rdf:type <https://hg.schaathun.net/armchar/schema#SpecialTraitClass>  ."
-- "?s <https://hg.schaathun.net/armchar/schema#isSpecialTrait> ?t  ."
