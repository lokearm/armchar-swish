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
-- 1.  Initially using the `traitclasstypeRule` to infer type from the
--     `arm:traitClass` property.
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

module ArM.Rules ( makeGraph
                 , prepareSchema, prepareResources 
                 ) where

import Swish.RDF.Graph

-- import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.Common
import ArM.Rules.RDFS
import ArM.Rules.FullGraph (prepareGraph)
import ArM.Rules.Resource (prepareResources)

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
makeGraph c0 _ res1 = ( prepareGraph . merge res1 . initialRules ) c0

-- | Apply simple rules which do not depend on the schema
initialRules :: RDFGraph -> RDFGraph
initialRules = fwdApplyList [ advclasstypeRule, traitclasstypeRule, bonusclassrule ] 
