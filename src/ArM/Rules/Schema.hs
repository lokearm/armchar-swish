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
fwdApplyListS schema rs g = addGraphs schema $ foldl addGraphs g $ map (`f` g) rs
     where f = fwdApplySimpleS schema
fwdApplyListSR :: RDFGraph -> [RDFRule] ->  RDFGraph -> RDFGraph
fwdApplyListSR schema rs g = if (g' == g) then g'
                     else fwdApplyListSR schema rs g'
                     where g' = fwdApplyListS schema rs g

prepareCS schema = fwdApplyListS schema traitRules 
                 . fwdApplyListS schema rdfstypeRules
                 . fwdApplyListSR schema rdfsRules

traitRules = map mkr [ "Ability"
                     , "Virtue"
                     , "Flaw"
                     , "PersonalityTrait"
                     , "Reputation"
                     , "Spell"
                     , "Art"
                     , "OtherTrait"
                     , "Characteristic" ]
    where mkr s = mkr' ("has" ++ s ++ "Rule")
                       (Res $ makeSN s) (Res $ makeSN $ "has" ++ s)
          mkr' s t p = makeCRule s g1 g2 where (g1,g2) = arcs t p

arcs t p = ( [ arc cVar htRes tVar, arc tVar typeRes t ],
             [ arc cVar p tVar ] ) 


