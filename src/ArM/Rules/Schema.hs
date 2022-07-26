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

import Control.Parallel.Strategies


-- prepareCS schema g = foldl addGraphs g $ fwdApplyMap rs gg
                 -- where gg = merge schema g
                       -- rs = copyRules  ++ traitRules  ++ rdfstypeRules
prepareCS schema = fwdApplyList traitRules 
                 . fwdApplyList rdfstypeRules
                 . merge schema

traitRules = traitRules1 ++ traitRules2
-- | Rules to infer subproperties of arm:hasTrait
traitRules1 = map mkr [ "Ability"
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
          mkr' s t p = makeCRule s g1 g2 where (g1,g2) = arcs1 t p
-- | Rules to infer arm:hasTrait from subproperties
traitRules2 = map mkr [ "Ability"
                     , "Virtue"
                     , "Flaw"
                     , "PersonalityTrait"
                     , "Reputation"
                     , "Spell"
                     , "Art"
                     , "OtherTrait"
                     , "Characteristic" ]
    where mkr s = mkr' ("has" ++ s ++ "IRule")
                       (Res $ makeSN s) (Res $ makeSN $ "has" ++ s)
          mkr' s t p = makeCRule s g1 g2 where (g1,g2) = arcs2 t p

arcs1 t p = ( [ arc cVar htRes tVar, arc tVar typeRes t ],
             [ arc cVar p tVar ] ) 
arcs2 t p = ( [ arc cVar p tVar, arc tVar typeRes t ],
             [ arc cVar htRes tVar ] ) 
