-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Resources
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoning rules using the resource ontology.
--
-----------------------------------------------------------------------------

module ArM.Rules.Resource where

import Swish.RDF.Ruleset
import qualified Data.Text as T
import Swish.Rule
import Swish.RDF.Graph
import ArM.Resources
import ArM.Rules.Aux

-- | Infere resource properties from class
prepareGraph = fwdApplyList [ rRule ]

rRule = makeRule "rRule" 
       "?s rdf:type ?t . ?t ?p ?o . ?p rdf:type <https://hg.schaathun.net/armchar/schema#TraitProperty> ."
       "?s ?p ?o ."
