-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Aux
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Generic and auxiliary functions for reasoning.
--
-----------------------------------------------------------------------------

module ArM.Rules.Aux where

import Swish.RDF.Ruleset as SRR
import Swish.Rule as SR
import Swish.RDF.Graph as SRG
import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Vocabulary.XSD

import Data.Set (fromList)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromString)
import ArM.Resources
import Swish.VarBinding (varBindingId) 

import Control.Parallel.Strategies



-- | Simple forward application of a rule
-- When this results in multiple graphs, these are added together
-- usign 'addGraphs' (via 'foldGraphs')
fwdApplySimple :: RDFRule ->  RDFGraph -> RDFGraph
fwdApplySimple r c = foldGraphs $ SR.fwdApply r [c]

-- | Add together a list of graphs.
-- This is a fold with 'addGraphs' and hence it assumes that
-- blank nodes are shared between the graphs.  This is the case
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
     foldl addGraphs g $ parMap rpar (`fwdApplySimple` g) rs

fwdApplyMap rs g = parMap rpar (`fwdApplySimple` g) rs

fwdApplyListR rs g = if (g' == g) then g'
                     else fwdApplyListR rs g'
                     where g' = fwdApplyList rs g

typeRes = Res rdfType 
subclassRes = Res rdfsSubClassOf 
tArc = arc sVar (Res rdfType) tVar 
lVar = (Var "l")
sVar = (Var "s")
tVar = (Var "t")
oVar = (Var "o")
pVar = (Var "p")
cVar = (Var "c")
csVar = (Var "cs")

gtRes = armRes "grantsTrait" 
htRes = armRes "hasTrait" 
csRes = armRes "CharacterSheet" 
labelRes = (Res rdfsLabel)

listToRDFGraph :: [RDFTriple] -> RDFGraph
listToRDFGraph = toRDFGraph .  fromList 

makeCRule :: String -> [RDFTriple] -> [RDFTriple] -> RDFRule
makeCRule s l1 l2 = makeRDFClosureRule ( makeSN s )
            [listToRDFGraph  l1]
            (listToRDFGraph  l2)
            varBindingId


-- | Query Graph to get an Object property from a resource
-- The first argument p is a property and the second one s is a resource ID.
-- The query will find a triple (s,p,x) and return all triples with x as
-- subject.
qgraph :: RDFLabel -> RDFLabel -> RDFGraph
qgraph p s = listToRDFGraph 
      [ arc s p (Var "id")
      , arc (Var "id") (Var "property") (Var "value")
      , arc (Var "property") labelRes (Var "label") ]

fwdApplyRules rs g = foldGraphs $ parMap rpar (`fwdApplySimple` g) rs
litInt i = TypedLit (T.pack $ show i) xsdInteger
