-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoning rules to augment Advancement resources
--
-----------------------------------------------------------------------------

module ArM.Rules.Advancement where

-- import qualified Swish.RDF.Query as Q
import Swish.RDF.Graph
import Swish.RDF.Ruleset (RDFRule)
import ArM.Resources
import ArM.Rules.Aux

-- import Control.Parallel.Strategies

-- | Prepare a character record graph.
-- This includes merging in the given schema
-- prepareRecord :: RDFGraph -> RDFGraph -> RDFGraph
-- prepareRecord schema = id

-- set awardsXP
modRule :: RDFRule
modRule = 
    makeCRule "modifierRuld"
      [ arc mVar typeRes (armRes "Modifier") 
      , arc mVar (armRes "hasProperty") (Var "prop") 
      , arc mVar (armRes "hasClass") (Var "class") 
      , arc mVar (armRes "hasValue") (Var "obj") 
      , arc cVar (armRes "hasTrait") tVar
      , arc tVar (armRes "hasModifier") mVar
      , arc (Var "adv") (armRes "advanceCharacter") cVar
      , arc (Var "adv") typeRes (Var "class")
      ]
      [ arc (Var "adv") (Var "prop") (Var "obj") ]
    where mVar = (Var "m")
