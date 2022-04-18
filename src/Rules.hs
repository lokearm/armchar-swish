module Rules ( prepareInitialCharacter
             , typedClosure  ) where

import Swish.RDF.Ruleset
import qualified Swish.QName as QN
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import Swish.Rule
import Swish.RDF.Graph
import ArM.Resources

-- Apply Rules
fwdApplySimple :: RDFRule ->  RDFGraph -> RDFGraph
fwdApplySimple r c = foldGraphs $ fwdApply r [c]
    where 

foldGraphs :: [RDFGraph] -> RDFGraph
foldGraphs [] = emptyRDFGraph
foldGraphs (x:xs) = foldl addGraphs x xs

fwdApplyMerge :: RDFRule ->  RDFGraph -> RDFGraph
fwdApplyMerge r c = addGraphs c $ fwdApplySimple r c

-- | Apply a list of rules to a graph
fwdApplyList rs g =
     foldl addGraphs g $ map (`fwdApplySimple` g) rs


-- Auxiliaries
newLName s = case (QN.newLName $ T.pack s) of
   (Nothing) -> QN.emptyLName
   (Just ln) -> ln

makeRule ln s1 s2 = makeN3ClosureSimpleRule 
    rulesNS ( newLName ln ) ( fromString s1 ) ( fromString s2 )
-- fromString s = ( fromText . T.pack ) s :: Builder

-- Rules
csRule = makeRule "csRule" 
    "?cs <https://hg.schaathun.net/armchar/schema#isCharacter> ?c . ?c ?p ?o ."
                  "?cs ?p ?o ."
advtypeRule = makeRule "advtypeRule" 
    "?s <https://hg.schaathun.net/armchar/schema#hasAdvancementType> ?c .  ?c rdfs:label ?l ."
    "?s <https://hg.schaathun.net/armchar/schema#hasAdvancementTypeString> ?l . "
traitclassRule = makeRule "traitclassRule" 
    ( "?s <https://hg.schaathun.net/armchar/schema#traitClass> ?c . "
    ++ "?c <https://hg.schaathun.net/armchar/schema#hasLabel> ?l ." )
    ( "?s <https://hg.schaathun.net/armchar/schema#traitClassString> ?l . "
    ++ "?s <https://hg.schaathun.net/armchar/schema#traitClass> ?c . "
    ++ "?c <https://hg.schaathun.net/armchar/schema#hasLabel> ?l ." )


subclassRule = makeRule "subclassRule" 
    "?s rdf:type ?t . ?t rdfs:subClassOf ?c ."
                  "?s rdf:type ?c ."


prepareInitialCharacter :: RDFGraph -> RDFGraph
prepareInitialCharacter = 
   fwdApplyList [ advtypeRule, traitclassRule, csRule ]

typedClosure :: RDFGraph -> RDFGraph
typedClosure = fwdApplyList [subclassRule]
