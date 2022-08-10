-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Persistence
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoner rules to extract triples for persistence.
--
-----------------------------------------------------------------------------

module ArM.Rules.Persistence where

import           ArM.Rules.Aux
import           ArM.Rules.RDFS
import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import           Swish.RDF.Vocabulary.RDF

import ArM.Resources

-- persistGraph' s g = persistGraph' $ G.merge s g
-- persistGraph' s g = foldGraphs $ Q.rdfQuerySubs vb tg
--     where vb = Q.rdfQueryFind qg g
--           qg = listToRDFGraph  [ G.arc sVar pVar cVar,
--                        G.arc pVar typeRes armPersistentProperty ]
--           tg = listToRDFGraph  [ G.arc sVar pVar cVar ]

p1 =  makeCRule "persistRule" 
    [ G.arc sVar pVar cVar,
      G.arc pVar typeRes armPersistentProperty ]
    [ G.arc sVar pVar cVar ]
persistRules = 
  [ p1
  , makeCRule "persistTraitRule" 
    [ G.arc sVar (armRes "advanceTrait") tVar
    , G.arc tVar pVar oVar
    , G.arc pVar typeRes armPersistentProperty 
    ]
    [ G.arc sVar (armRes "advanceTrait") tVar
    , G.arc tVar pVar oVar
    ]
  , makeCRule "persistItemRule" 
    [ G.arc sVar (armRes "changePossession") tVar
    , G.arc tVar pVar oVar
    , G.arc pVar typeRes armPersistentProperty 
    ]
    [ G.arc sVar (armRes "changePossession") tVar
    , G.arc tVar pVar oVar
    ]
  ]
p2 id = makeCRule "persistedRule" 
    [ G.arc id pVar cVar,
      G.arc pVar typeRes armPersistentProperty ]
    [ G.arc id pVar cVar ]
persistedRules id =
  [ p2 id
  , makeCRule "persistedTraitRule" 
    [ G.arc id (armRes "advanceTrait") tVar
    , G.arc tVar pVar oVar
    , G.arc pVar typeRes armPersistentProperty 
    ]
    [ G.arc id (armRes "advanceTrait") tVar
    , G.arc tVar pVar oVar
    ]
  , makeCRule "persistedTraitRule" 
    [ G.arc id (armRes "changePossession") tVar
    , G.arc tVar pVar oVar
    , G.arc pVar typeRes armPersistentProperty 
    ]
    [ G.arc id (armRes "changePossession") tVar
    , G.arc tVar pVar oVar
    ]
  ]
persistedGraph g s = fwdApplyRules (persistedRules s) g
persistGraph s g = fwdApplyRules persistRules  $ applyRDFS $ G.merge s g

persistedChar g s = fwdApplyRules [ p2 s ] g
persistChar s g = fwdApplyRules [ p1 ] $ applyRDFS $ G.merge s g

