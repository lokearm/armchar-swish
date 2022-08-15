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
-- The schema is prepared by the `prepareSchema` function in this module.
-- The resource graph by functions in `ArM.Rules.Resource`.
--
-- The character graph is agumented in several steps.
-- 1.  Initially using the `traitclasstypeRule`.
-- 2.  When the resources have been added, by rules in `ArM.Rules.FullGraph`.
-- 3.  Advancement is applied using internal Haskell representations and 
--     not RDF graphs.  These functions are in `ArM.Character.Character`.
-- 4.  Individual character sheets, after advancement, are further 
--     augmented by rules in `ArM.Rules.Record`.
--
-- Rules are also applied to extract properties to be stored upon
-- update.  These rules are in `ArM.Rules.Persistence`.
--
-- Auxiliary and shared functions for the reasoners are provided by
-- `ArM.Rules.Aux` and `ArM.Rules.Common`.
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
makeGraph c0 s1 res1 = ( prepareGraph . merge res1 . prepareCharGraph ) c0

-- | Compute the three graph to be kept in software transactional
-- memory.
makeGraphs :: 
    (RDFGraph,RDFGraph,RDFGraph) 
    -- ^ Raw graphs as read from file (character graph,schema,resources)
    ->  (RDFGraph,RDFGraph,RDFGraph) 
    -- ^ Graphs augmented by the reasoner (character graph,schema,resources)
makeGraphs (c0,s0,res0) =
      s1 `par` res1 `pseq` ( c2, s1, res1 )
    where c1 = fwdApplyList [ traitclasstypeRule ] c0
          s1 = prepareSchema s0
          res1 = prepareResources $ res0 `merge` s1
          c2 = prepareGraph $ merge res1 c1
