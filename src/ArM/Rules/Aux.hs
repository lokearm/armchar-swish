-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Aux
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Generic and auxiliary functions for reasoning
--
-----------------------------------------------------------------------------

module ArM.Rules.Aux where

import Swish.RDF.Ruleset
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromString)
import Swish.Rule
import Swish.RDF.Graph (RDFGraph, emptyRDFGraph, addGraphs)
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

fwdApplyListR rs g = if (g' == g) then g'
                     else fwdApplyListR rs g'
                     where g' = fwdApplyList rs g


-- | Convenience function to make a Rule from Strings of Notation3 data.
makeRule ln s1 s2 = makeN3ClosureSimpleRule 
    rulesNS ( newLName ln ) ( fromString s1 ) ( fromString s2 )
