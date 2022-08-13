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
-- Rules used include
-- 1.  String properties are added to avoid having to handle simple objects
--     at the client.
--
-----------------------------------------------------------------------------

module ArM.Rules.FullGraph (prepareGraph) where

import Swish.RDF.Graph
import ArM.Rules.Common
import ArM.Resources
import ArM.Rules.Aux
import ArM.Rules.RDFS

-- | Final inference to be done after merging character data and resources
-- This is expensive, and may need caution.
-- It will be applied every time the graph changes, and the graph is large
prepareGraph = fwdApplyList (stringPropertyRule:vfScoreRules)
             . fwdApplyListR [ advancevfgrantRule, spectraitRule, rRule, pRule ]
             . applyRDFS


rRule = makeCRule "rRule" l1 l2
    where l1 = [ arc sVar ( armRes  "traitClass" ) tVar,
               arc tVar pVar oVar,
               arc pVar typeRes ( armRes  "TraitProperty" )  ]
          l2 = [arc sVar pVar oVar]
pRule = makeCRule "pRule" l1 l2
    where l1 = [ arc sVar ( armRes  "traitClass" ) tVar,
               arc tVar pVar oVar,
               arc pVar typeRes ( armRes  "PossessionProperty" )  ]
          l2 = [arc sVar pVar oVar]


vfScoreRules = 
   [ makeCRule "flawScoreRule"
       [ arc cVar (armRes "buyVirtueFlaw") sVar
       , arc sVar typeRes (armRes "majorFlaw") ]
       [ arc sVar (armRes "hasScore") (litInt (-3)) ]
   , makeCRule "minorFlawScoreRule"
       [ arc cVar (armRes "buyVirtueFlaw") sVar
       , arc sVar typeRes (armRes "minorFlaw") ]
       [ arc sVar (armRes "hasScore") (litInt (-1)) ]
   , makeCRule "virtueScoreRule"
       [ arc cVar (armRes "buyVirtueFlaw") sVar
       , arc sVar typeRes (armRes "majorVirtue") ]
       [ arc sVar (armRes "hasScore") (litInt (3)) ]
   , makeCRule "minorVirtueScoreRule"
       [ arc cVar (armRes "buyVirtueFlaw") sVar
       , arc sVar typeRes (armRes "minorVirtue") ]
       [ arc sVar (armRes "hasScore") (litInt (1)) ]
   , makeCRule "houseVirtueRule"
       [ arc cVar (armRes "hasHouseVirtue") sVar ]
       [ arc sVar (armRes "hasScore") (litInt (0)) ]
   ]

spectraitRule = makeCRule  "spectraitRule" 
      [ tArc
      , arc tVar typeRes ( armRes  "SpecialTraitClass" ) ] 
      [ arc sVar ( armRes  "isSpecialTrait" ) tVar ]


-- | apply grantsTrait to an Advancement
advancevfgrantRule = makeCRule  "advancevfgrantRule" 
     [ arc sVar (armRes "advanceTrait") oVar,
       arc oVar typeRes tVar,   -- o a t
       arc sVar typeRes ( armRes "CharacterAdvancement" ),
       arc tVar gtRes cVar ]
     [ arc sVar (armRes "advanceTrait") cVar ]
