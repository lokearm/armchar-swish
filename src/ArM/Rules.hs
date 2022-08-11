{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Apply the reasoner to make augmented graphs.
-- The actual rules used are defined and (will be) documented in
-- `ArM.Rules.*`.
--
-----------------------------------------------------------------------------

module ArM.Rules ( makeGraph, makeGraphs ) where

import Swish.RDF.Graph

import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.RDFS
import ArM.Rules.Initial
import ArM.Rules.FullGraph (prepareGraph)
import ArM.Rules.Resource (prepareResources)

import Control.Parallel


-- | Apply standard RDFS rules to elaborate the schema
-- This is used only once, so it may be allowed to be costly.
prepareSchema :: RDFGraph -> RDFGraph
prepareSchema = fwdApplyListR rdfsRules

-- | Recompute the character graph to be kept in software transactional
-- memory.  This is used when the raw character graph changes.
-- For initial computation, `makeGraphs` should be used instead.
makeGraph :: RDFGraph -- ^ The raw character graph
             ->  RDFGraph -- ^ The pre-processed schema graph
             ->  RDFGraph -- ^ The pre-processed resource graph
             ->  RDFGraph -- ^ The derived character graph
makeGraph c0 s1 res1 = ( prepareGraph . merge res1 
    . prepareInitialCharacter . merge s1 . prepareCharGraph ) c0

-- | Compute the three graph to be kept in software transactional
-- memory.
makeGraphs :: 
    (RDFGraph,RDFGraph,RDFGraph) 
    -- ^ Raw graphs as read from file (character graph,schema,resources)
    ->  (RDFGraph,RDFGraph,RDFGraph) 
    -- ^ Graphs augmented by the reasoner (character graph,schema,resources)
makeGraphs (c0,s0,res0) =
      s1 `par` res1 `par` res2 `par` c2 `pseq` ( c3, s1, res2 )
    where c1 = prepareCharGraph c0
          s1 = prepareSchema s0
          res1 = prepareResources res0
          c2 = prepareInitialCharacter $ merge s1 c1
          c3 = prepareGraph $ merge res1 c2 
          res2 = applyRDFS $ merge s1 res1
