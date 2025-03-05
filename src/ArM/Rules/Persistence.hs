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
import           Swish.RDF.Graph
import Swish.RDF.Ruleset (RDFRule)
-- import qualified Swish.RDF.Query as Q
-- import           Swish.RDF.Vocabulary.RDF

import ArM.Swish.Resources

-- persistGraph' s g = persistGraph' $ merge s g
-- persistGraph' s g = foldGraphs $ Q.rdfQuerySubs vb tg
--     where vb = Q.rdfQueryFind qg g
--           qg = listToRDFGraph  [ arc sVar pVar cVar,
--                        arc pVar typeRes armPersistentProperty ]
--           tg = listToRDFGraph  [ arc sVar pVar cVar ]

p1 :: RDFRule
p1 =  makeCRule "persistRule" 
    [ arc sVar pVar cVar,
      arc pVar typeRes armPersistentProperty ]
    [ arc sVar pVar cVar ]
persistRules :: [RDFRule]
persistRules = 
  [ p1
  , makeCRule "persistTraitRule" 
    [ arc sVar (armRes "advanceTrait") tVar
    , arc tVar pVar oVar
    , arc pVar typeRes armPersistentProperty 
    ]
    [ arc sVar (armRes "advanceTrait") tVar
    , arc tVar pVar oVar
    ]
  ]
p2 :: RDFLabel -> RDFRule
p2 k = makeCRule "persistedRule" 
    [ arc k pVar cVar,
      arc pVar typeRes armPersistentProperty ]
    [ arc k pVar cVar ]
persistedRules :: RDFLabel -> [RDFRule]
persistedRules k =
  [ p2 k
  , makeCRule "persistedTraitRule" 
    [ arc k (armRes "advanceTrait") tVar
    , arc tVar pVar oVar
    , arc pVar typeRes armPersistentProperty 
    ]
    [ arc k (armRes "advanceTrait") tVar
    , arc tVar pVar oVar
    ]
  ]
persistedGraph :: RDFGraph -> RDFLabel -> RDFGraph
persistedGraph g s = fwdApplyRules (persistedRules s) g
persistGraph :: RDFGraph -> RDFGraph -> RDFGraph
persistGraph s g = fwdApplyRules persistRules  $ applyRDFS $ merge s g

persistedChar :: RDFGraph -> RDFLabel -> RDFGraph
persistedChar g s = fwdApplyRules [ p2 s ] g
persistChar :: RDFGraph -> RDFGraph -> RDFGraph
persistChar s g = fwdApplyRules [ p1 ] $ applyRDFS $ merge s g

