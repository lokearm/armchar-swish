{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Rules and Rules Application for ArM Character Sheets. 
--
-----------------------------------------------------------------------------

module ArM.Rules 
  -- ( prepareInitialCharacter)
  where

import Swish.RDF.Ruleset
import qualified Data.Text as T
import Swish.Rule
import Swish.RDF.Graph
import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.RDFS
import qualified ArM.Rules.Schema as RS

import Control.Parallel

import qualified ArM.Rules.Resource as RR
import qualified ArM.Rules.FullGraph as RG

-- Initial Character Rules


-- | Infer a string representation of the Advancement Type
-- (Reading, Practice, Exposure, etc.)
advtypeRule = makeCRule "advtypeRule" 
    [ arc sVar hasAdvancementType cVar, arc cVar labelRes lVar ]
    [ arc sVar hasAdvancementTypeString lVar ]

-- | Infer a string representation of the Trait Class of each Trait Advancement
traitclassRule = makeCRule "traitclassRule" 
    [ arc sVar (armRes "traitClass") cVar,
      arc cVar (armRes "hasLabel") oVar ]
    [ arc sVar (armRes "traitClassString") oVar ]
traitclasstypeRule = makeCRule "traitclasstypeRule" 
       [ arc sVar ( armRes "traitClass" ) tVar ]
       [ arc sVar typeRes tVar ]

-- | Add indices used for sorting advancements
advancementindexRule = makeCRule "advancementindexRule" 
    [ tArc, arc tVar (armRes "hasAdvancementIndex") cVar ]
    [ arc sVar (armRes "hasAdvancementIndex") cVar ]

-- | Initial inferences on the character data, to be applied without
-- the schema
prepareCharGraph :: RDFGraph -> RDFGraph
prepareCharGraph = fwdApplyList [ traitclasstypeRule ]

-- | Make all necessary inferences before retrieving character data
prepareInitialCharacter :: RDFGraph -> RDFGraph
prepareInitialCharacter = 
   fwdApplyList (
      advtypeRule:traitclassRule:advancementindexRule:rdfstypeRules )

-- | Apply standard RDFS rules to elaborate the schema
-- This is used only once, so it may be allowed to be costly.
prepareSchema :: RDFGraph -> RDFGraph
prepareSchema = fwdApplyListR rdfsRules

-- | Final inference to be done after merging with the resource graph
-- This is expensive, and may need caution.
-- It will be applied every time the graph changes, and the graph
-- is large
prepareGraph = RG.prepareGraph . applyRDFS

prepareResources = RR.prepareResources . applyRDFS
                 . fwdApplyList [ traitclasstypeRule ]

-- | Prepare a character sheet. 
prepareRecord schema = RS.prepareCS schema 
                 . fwdApplyList [ traitclasstypeRule ]

makeGraphs (c0,s0,res0) = s1 `par` res1 `par` res2 `par` c2 `pseq` ( c3, s1, res2 )
    where c1 = prepareCharGraph c0
          s1 = prepareSchema s0
          res1 = prepareResources res0
          c2 = prepareInitialCharacter $ merge s1 c1
          c3 = prepareGraph $ merge res1 c2 
          res2 = applyRDFS $ merge s1 res1
