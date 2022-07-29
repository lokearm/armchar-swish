-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.FullGraph
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoning rules on the full graph using the resource ontology.
--
-----------------------------------------------------------------------------

module ArM.Rules.FullGraph (prepareGraph) where

import Swish.RDF.Graph
import ArM.Resources
import ArM.Rules.Aux

-- | Infere resource properties from class
prepareGraph = fwdApplyListR [ advancevfgrantRule, grantRule, spectraitRule, rRule ]

rRule = makeCRule "rRule" l1 l2
    where l1 = [ arc sVar ( armRes  "traitClass" ) tVar,
               arc tVar pVar oVar,
               arc pVar typeRes ( armRes  "TraitProperty" )  ]
          l2 = [arc sVar pVar oVar]


spectraitRule = makeCRule  "spectraitRule" 
      [ tArc
      , arc tVar typeRes ( armRes  "SpecialTraitClass" ) ] 
      [ arc sVar ( armRes  "isSpecialTrait" ) tVar ]

-- | apply grantsTrait to a CharacterSheet
grantRule = makeCRule  "grantRule" 
     [ arc sVar htRes oVar,     -- s hasTrait o
       arc oVar typeRes tVar,   -- o a t
       arc sVar typeRes csRes,  -- s a CharacterSheet
       arc tVar gtRes cVar ]    -- o grantsTrait c
     [ arc sVar htRes cVar ]    -- s

-- | apply grantsTrait to an Advancement
advancevfgrantRule = makeCRule  "advancevfgrantRule" 
     [ arc sVar (armRes "advanceTrait") oVar,
       arc oVar typeRes tVar,   -- o a t
       arc sVar typeRes ( armRes "CharacterAdvancement" ),
       arc tVar gtRes cVar ]
     [ arc sVar (armRes "advanceTrait") cVar ]
