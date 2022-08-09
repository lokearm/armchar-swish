{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Initial
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Rules and Rules Application to be applied on the initial character
-- sheet without the resource ontology.
--
-- Two rules are applied at this stage
-- 1.  RDFS type inferences, expanding subclasses and subproperties.
-- 2.  Advancement indices are copied from classes to instances.
--
-- Before the schema is added rdf types are derived from the traitClass property.
--
-----------------------------------------------------------------------------

module ArM.Rules.Initial where

import Swish.RDF.Graph
import ArM.Resources

import ArM.Rules.Aux
import ArM.Rules.Common
import ArM.Rules.RDFS

-- | Infer a string representation of the Advancement Type
-- (Reading, Practice, Exposure, etc.)
advtypeRule = makeCRule "advtypeRule" 
    [ arc sVar hasAdvancementType cVar, arc cVar labelRes lVar ]
    [ arc sVar hasAdvancementTypeString lVar ]

-- | Add indices used for sorting advancements
advancementindexRule = makeCRule "advancementindexRule" 
    [ tArc, arc tVar (armRes "hasAdvancementIndex") cVar ]
    [ arc sVar (armRes "hasAdvancementIndex") cVar ]

-- | Initial inferences on the character data, to be applied without
-- the schema
prepareCharGraph :: RDFGraph -> RDFGraph
prepareCharGraph = fwdApplyList [ traitclasstypeRule ]

-- | Inference on the character data merged with the schema
prepareInitialCharacter :: RDFGraph -> RDFGraph
prepareInitialCharacter = 
   fwdApplyList ( advancementindexRule:rdfstypeRules )
