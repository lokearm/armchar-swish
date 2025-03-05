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

module ArM.Rules.Advancement ( prepareAdvancements, mod1Rule, mod2Rule ) where

-- import qualified Swish.RDF.Query as Q
import Swish.RDF.Graph
import Swish.RDF.Ruleset (RDFRule)
import ArM.Swish.Resources
import ArM.Rules.Aux

-- import Control.Parallel.Strategies


-- | Add XP awards to pregame advancements
-- This should be applied to the character graph prior to CharGen,
-- but after RDFS and the inheritanceRule.
-- This is very special in that it modifies the Character resource,
-- rather than CharacterSheet resources.  This is necessary to 
-- modify the Advancements before the CharacterSheet objects are
-- created.
prepareAdvancements :: RDFGraph -> RDFGraph
prepareAdvancements = fwdApplySimple mod2Rule 
                    . fwdApplySimple mod1Rule  

mod1Rule :: RDFRule
mod1Rule = 
    makeCRule "mod1rule"
      [ arc adv typeRes (armRes "BaseAdvancement")
      , arc adv (armRes "advanceCharacter") cVar
      , arc adv (armRes "advanceTrait") trait
      , arc trait (armRes "hasModifier") mVar
      ]
      [ arc cVar (armRes "hasModifier") mVar ]
    where adv = Var "adv"
          trait = Var "trait"
          mVar = Var "mod"

mod2Rule :: RDFRule
mod2Rule = 
    makeCRule "mod2rule"
      [ arc cVar (armRes "hasModifier") mVar
      , arc cVar (typeRes) (armRes "Character")
      , arc adv (armRes "advanceCharacter") cVar
      , arc mVar (armRes "hasProperty") (Var "prop") 
      , arc mVar (armRes "hasClass") (Var "class") 
      , arc mVar (armRes "hasValue") (Var "obj") 
      , arc (Var "adv") typeRes (Var "class")
      ]
      [ arc (Var "adv") (Var "prop") (Var "obj") ]
    where adv = Var "adv"
          mVar = Var "mod"
