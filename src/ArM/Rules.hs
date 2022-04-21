{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Rules and Rules Application for ArM Character Sheets. 
--
-----------------------------------------------------------------------------

module ArM.Rules 
  -- ( prepareInitialCharacter)
  where

import Swish.RDF.Ruleset
import qualified Data.Text as T
-- import Data.Text.Lazy.Builder
import Swish.Rule
import Swish.RDF.Graph
import ArM.Resources
import ArM.Rules.Aux




-- Initial Character Rules

-- | Infer character sheet properties from character properties
csRule = makeRule "csRule" 
    "?cs <https://hg.schaathun.net/armchar/schema#isCharacter> ?c . ?c ?p ?o ."
                  "?cs ?p ?o ."
-- | Infer a string representation of the Advancement Type (Reading, Practice, Exposure, etc.)
advtypeRule = makeRule "advtypeRule" 
    "?s <https://hg.schaathun.net/armchar/schema#hasAdvancementType> ?c .  ?c rdfs:label ?l ."
    "?s <https://hg.schaathun.net/armchar/schema#hasAdvancementTypeString> ?l . "
-- | Infer a string representation of the Trait Class of each Trait Advancement
traitclassRule = makeRule "traitclassRule" 
    ( "?s <https://hg.schaathun.net/armchar/schema#traitClass> ?c . "
    ++ "?c <https://hg.schaathun.net/armchar/schema#hasLabel> ?l ." )
    "?s <https://hg.schaathun.net/armchar/schema#traitClassString> ?l . "
traitclasstypeRule = makeRule "traitclasstypeRule" 
       "?s <https://hg.schaathun.net/armchar/schema#traitClass> ?t . "
       "?s rdf:type ?t . "

-- | RDFS Rules
rdfsRules = [
    makeRule "subclassRule" 
       "?s rdfs:subClassOf ?t . ?t rdfs:subClassOf ?c ."
       "?s rdfs:subClassOf ?c ."
    , makeRule "subpropRule" 
       "?s rdfs:subPropertyOf ?t . ?t rdfs:subPropertyOf ?c ."
       "?s rdfs:subPropertyOf ?c ."
       ]

-- | Rules to infer additional types and properties from subclass 
-- and subproperty relations (using RDFS vocabulary).
rdfstypeRules = [
    makeRule "subclasstypeRule"
       "?s rdf:type ?t . ?t rdfs:subClassOf ?c ."
       "?s rdf:type ?c ."
    , makeRule "subpropinstanceRule" 
       "?s ?p ?o . ?p rdfs:subPropertyOf ?p2 ."
       "?s ?p2 ?o ."
       ]



-- | Add indices used for sorting advancements
advancementindexRule = makeRule "advancementindexRule" 
    ( "?s rdf:type ?t . "
    ++ "?t <https://hg.schaathun.net/armchar/schema#hasAdvancementIndex> ?c ." 
    )
    "?s <https://hg.schaathun.net/armchar/schema#hasAdvancementIndex> ?c ." 

-- | Add indices used for sorting advancements
initialsheetRule = makeRule "initialsheetRule" 
    "?c <https://hg.schaathun.net/armchar/schema#hasInitialSheet>  ?s . "
    ( "?s <https://hg.schaathun.net/armchar/schema#isCharacter> ?c ." 
    ++ "?s rdf:type <https://hg.schaathun.net/armchar/schema#CharacterSheet> ."  )

-- Make all necessary inferences before retrieving character data
prepareInitialCharacter :: RDFGraph -> RDFGraph
prepareInitialCharacter = 
   fwdApplyList (
      csRule:advtypeRule:traitclassRule:advancementindexRule:rdfstypeRules )

-- | Apply standard RDFS rules to elaborate the schema
prepareSchema :: RDFGraph -> RDFGraph
prepareSchema = fwdApplyListR rdfsRules

-- | Initial inferences on the character data, to be applied without
-- the schema
prepareCS :: RDFGraph -> RDFGraph
prepareCS = 
   fwdApplyList [ initialsheetRule, traitclasstypeRule ]

spectraitRule = 
    makeRule "spectraitRule"
       "?s rdf:type ?t . ?t rdf:type <https://hg.schaathun.net/armchar/schema#SpecialTraitClass>  ."
       "?s <https://hg.schaathun.net/armchar/schema#isSpecialTrait> ?t  ."

-- | Make inferences on the joint graph including resources
prepareGraph = 
   fwdApplyListR ( spectraitRule:rdfstypeRules ++ rdfsRules ) 
