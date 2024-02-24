-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoning rules to augment Advancement resources
--
-----------------------------------------------------------------------------

module ArM.Rules.Advancement where

import qualified Swish.RDF.Query as Q
import Swish.VarBinding  (vbMap)
import Swish.RDF.Graph
-- import Swish.RDF.Vocabulary.RDF
-- import Swish.RDF.Vocabulary.XSD
import Swish.RDF.Ruleset (RDFRule)
import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.Common
import ArM.Rules.RDFS
import Data.Maybe (fromJust)
-- import Data.List (sort)

-- import Control.Parallel.Strategies

-- | Prepare a character record graph.
-- This includes merging in the given schema
prepareRecord :: RDFGraph -> RDFGraph -> RDFGraph
prepareRecord schema = id

-- set awardsXP
