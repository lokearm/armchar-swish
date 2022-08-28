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
import Swish.RDF.Graph 
import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Vocabulary.XSD

import Data.Set (fromList)
import qualified Data.Text as T
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

fwdApplyList :: [RDFRule] -> RDFGraph -> RDFGraph
fwdApplyList rs g =
     foldl addGraphs g $ parMap rpar (`fwdApplySimple` g) rs

fwdApplyMap :: [RDFRule] -> RDFGraph -> [RDFGraph]
fwdApplyMap rs g = parMap rpar (`fwdApplySimple` g) rs

fwdApplyListR :: [RDFRule] -> RDFGraph -> RDFGraph
fwdApplyListR rs g = if (g' == g) then g'
                     else fwdApplyListR rs g'
                     where g' = fwdApplyList rs g

typeRes :: RDFLabel
typeRes = Res rdfType 
subclassRes :: RDFLabel
subclassRes = Res rdfsSubClassOf 
tArc :: RDFTriple
tArc = arc sVar (Res rdfType) tVar 
lVar :: RDFLabel
lVar = (Var "l")
sVar :: RDFLabel
sVar = (Var "s")
tVar :: RDFLabel
tVar = (Var "t")
oVar :: RDFLabel
oVar = (Var "o")
pVar :: RDFLabel
pVar = (Var "p")
cVar :: RDFLabel
cVar = (Var "c")
csVar :: RDFLabel
csVar = (Var "cs")

gtRes :: RDFLabel
gtRes = armRes "grantsTrait" 
htRes :: RDFLabel
htRes = armRes "hasTrait" 
csRes :: RDFLabel
csRes = armRes "CharacterSheet" 
labelRes :: RDFLabel
labelRes = (Res rdfsLabel)

listToRDFGraph :: [RDFTriple] -> RDFGraph
listToRDFGraph = toRDFGraph .  fromList 

makeCRule :: String -> [RDFTriple] -> [RDFTriple] -> RDFRule
makeCRule s l1 l2 = makeRDFClosureRule ( makeSN s )
            [listToRDFGraph  l1]
            (listToRDFGraph  l2)
            varBindingId


fwdApplyRules :: [RDFRule] -> RDFGraph -> RDFGraph
fwdApplyRules rs g = foldGraphs $ parMap rpar (`fwdApplySimple` g) rs

-- | Make an RDF label of type xsd:integer
litInt i = TypedLit (T.pack $ show i) xsdInteger


-- | Make an RDF label to hold a string.  This is currently made untyped
-- for the sake of simplicity.
litString i = Lit (T.pack i) 
-- litString i = TypedLit (T.pack i) xsdString

rdfToInt :: RDFLabel -> Maybe Int
rdfToInt = fromRDFLabel
rdfToString :: RDFLabel -> Maybe String
rdfToString = fromRDFLabel

intFromRDF :: RDFLabel -> Int
intFromRDF x = fi i
   where i = rdfToInt x
         s = rdfToString x
         fi Nothing = fs s
         fi (Just y) = y
         fs (Just y) = read y
