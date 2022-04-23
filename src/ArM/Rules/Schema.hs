-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Schema
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoning rules using the schema ontology.
--
-----------------------------------------------------------------------------

module ArM.Rules.Schema where

import Swish.RDF.Ruleset
import qualified Data.Text as T
import Swish.Rule
import Swish.RDF.Graph
import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Vocabulary.XSD
import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.RDFS
import Swish.VarBinding (varBindingId) 

-- | Simple forward application of a rule
-- When this results in multiple graphs, this are added together
-- usign 'addGraphs' (via 'foldGraphs')
fwdApplySimpleS :: RDFGraph -> RDFRule ->  RDFGraph -> RDFGraph
fwdApplySimpleS schema r cg = foldGraphs $ fwdApply r [schema,cg]

fwdApplyListS :: RDFGraph -> [RDFRule] ->  RDFGraph -> RDFGraph
fwdApplyListS schema rs g = foldl addGraphs g $ map (`f` g) rs
     where f = fwdApplySimpleS schema
fwdApplyListSR :: RDFGraph -> [RDFRule] ->  RDFGraph -> RDFGraph
fwdApplyListSR schema rs g = if (g' == g) then g'
                     else fwdApplyListSR schema rs g'
                     where g' = fwdApplyListS schema rs g

prepareCS schema = fwdApplyListS schema traitRules 
                 . fwdApplyListS schema rdfstypeRules
                 . fwdApplyListSR schema rdfsRules

traitRules = [ mkr "abRule" (Res $ makeSN "Ability") (Res $ makeSN "hasAbiity")
             , mkr "vRule" (Res $ makeSN "Virtue") (Res $ makeSN "hasVirtue")
             , mkr "fRule" (Res $ makeSN "Flaw") (Res $ makeSN "hasFlaw")
             , mkr "ptRule" (Res $ makeSN "PersonalityTrait")
                            (Res $ makeSN "hasPersonalityTrait")
             , mkr "ptRule" (Res $ makeSN "Reputation") 
                            (Res $ makeSN "hasReputation")
             , mkr "spRule" (Res $ makeSN "Spell") (Res $ makeSN "hasSpell")
             , mkr "artRule" (Res $ makeSN "Art") (Res $ makeSN "hasArt")
             , mkr "otRule" (Res $ makeSN "OtherTrait")
                            (Res $ makeSN "hasOtherTrait")
             , mkr "chRule" (Res $ makeSN "Characteristic")
                            (Res $ makeSN "hasCharacteristic")
             ]
mkr s t p = makeCRule s g1 g2
   where (g1,g2) = arcs t p
arcs t p = ( [ arc cVar htRes tVar, arc tVar typeRes t ],
             [ arc cVar p tVar ] ) 


