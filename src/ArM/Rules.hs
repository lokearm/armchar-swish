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
import qualified Swish.QName as QN
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import Swish.Rule
import Swish.RDF.Graph
import ArM.Resources

-- | Simple forward application of a rule
-- When this results in multiple graphs, this are added together
-- usign 'addGraphs' (via 'foldGraphs')
fwdApplySimple :: RDFRule ->  RDFGraph -> RDFGraph
fwdApplySimple r c = foldGraphs $ fwdApply r [c]
    where 

-- | Add together a list of graphs.
-- This is a fold with 'addGraphs' and hence it assumes that
-- blind nodes are shared between the graphs.  This is the case
-- when the graphs are the result of rule applications to the same
-- base graph.
foldGraphs :: [RDFGraph] -> RDFGraph
foldGraphs [] = emptyRDFGraph
foldGraphs (x:xs) = foldl addGraphs x xs

-- | Forward apply a rule and add the result with the original graph
fwdApplyMerge :: RDFRule ->  RDFGraph -> RDFGraph
fwdApplyMerge r c = addGraphs c $ fwdApplySimple r c

-- | Apply a list of rules to a graph
fwdApplyList rs g =
     foldl addGraphs g $ map (`fwdApplySimple` g) rs


-- | Define a local name from a String
newLName s = case (QN.newLName $ T.pack s) of
   (Nothing) -> QN.emptyLName
   (Just ln) -> ln

-- | Convenience function to make a Rule from Strings of Notation3 data.
makeRule ln s1 s2 = makeN3ClosureSimpleRule 
    rulesNS ( newLName ln ) ( fromString s1 ) ( fromString s2 )

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
    ( "?s <https://hg.schaathun.net/armchar/schema#traitClassString> ?l . "
    ++ "?s <https://hg.schaathun.net/armchar/schema#traitClass> ?c . "
    ++ "?c <https://hg.schaathun.net/armchar/schema#hasLabel> ?l ." )

-- | Infer additional types from subclass relationships 
subclassRule = makeRule "subclassRule" 
    "?s rdf:type ?t . ?t rdfs:subClassOf ?c ."
                  "?s rdf:type ?c ."
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
   fwdApplyList [ csRule, subclassRule, advtypeRule, traitclassRule, advancementindexRule ]
   . fwdApplyList [ initialsheetRule ]

