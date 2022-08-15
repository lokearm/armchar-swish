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
-- 2.  Infer RDF types from the arm:traitClass property.
--     Trait and Possession instances has a unique arm:traitClass property
--     which defines the class to which they directly belong.
-- 3.  Infer cost (score) of virtues and flaws.
-- 4.  Infer that advancements advance traits granted by virtues and flaws
-- 5.  Infer special traits.
--     **TODO** What is this used for?
--
-- RDFS rules are also applied.
--     
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
prepareGraph = fwdApplyList (advancementindexRule:stringPropertyRule:vfScoreRules)
             . fwdApplyListR [ advancevfgrantRule,
                               bonus1rule, bonus2rule,
                               spectraitRule, rRule, pRule ]
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

bonus1rule = makeCRule  "bonus1rule" 
     [ arc sVar (armRes "grantsBonus") oVar,
       arc sVar typeRes tVar, 
       arc tVar (armRes "grantsBonusScore") (Var "score") ]
     [ arc oVar (armRes "hasScore") (Var "score") ]
bonus2rule =  makeCRule  "bonus2rule" 
     [ arc sVar (armRes "advanceTrait") oVar
     , arc oVar (armRes "grantsBonus") cVar
     , arc sVar typeRes ( armRes "CharacterAdvancement" )
     , arc tVar gtRes cVar ]
     [ arc sVar (armRes "advanceTrait") cVar ]

-- | Add indices used for sorting advancements
advancementindexRule = makeCRule "advancementindexRule" 
    [ tArc, arc tVar (armRes "hasAdvancementIndex") cVar ]
    [ arc sVar (armRes "hasAdvancementIndex") cVar ]
