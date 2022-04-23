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
--
-- | Simple forward application of a rule
-- When this results in multiple graphs, this are added together
-- usign 'addGraphs' (via 'foldGraphs')
fwdApplySimpleS :: RDFGraph -> RDFRule ->  RDFGraph -> RDFGraph
fwdApplySimpleS schema r cg = foldGraphs $ fwdApply r [schema,cg]

fwdApplyListS :: RDFGraph -> [RDFRule] ->  RDFGraph -> RDFGraph
fwdApplyListS schema rs g = foldl addGraphs g $ map (`f` g) rs
     where f = fwdApplySimpleS schema

