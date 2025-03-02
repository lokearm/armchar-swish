{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Common
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Some rules which are used at many different stages of the reasoning.
--
-----------------------------------------------------------------------------

module ArM.Rules.Common where

import Swish.RDF.Graph
import ArM.Swish.Resources
import ArM.Rules.Aux
import Swish.RDF.Ruleset (RDFRule)

-- | Infer rdf:type from arm:traitClass
traitclasstypeRule :: RDFRule
traitclasstypeRule = makeCRule "traitclasstypeRule" 
       [ arc sVar ( armRes "traitClass" ) tVar ]
       [ arc sVar typeRes tVar ]
advclasstypeRule :: RDFRule
advclasstypeRule = makeCRule "advclasstypeRule" 
       [ arc sVar ( armRes "advancementClass" ) tVar ]
       [ arc sVar typeRes tVar ]
charclasstypeRule :: RDFRule
charclasstypeRule = makeCRule "charclasstypeRule" 
       [ arc sVar ( armRes "characterClass" ) tVar ]
       [ arc sVar typeRes tVar ]

-- | Many Object Properties are used only internally, and have an
-- associate property of string type for display purposes.  These
-- string properties are inferred by this rule.
stringPropertyRule :: RDFRule
stringPropertyRule = makeCRule "stringRule1"
       [ arc cVar pVar oVar
       , arc pVar (armRes "hasStringProperty") ( Var "p2" )
       , arc oVar (armRes "hasLabel") sVar ]
       [ arc cVar (Var "p2") sVar ]

bonusclassrule :: RDFRule
bonusclassrule = makeCRule "bonusclassrule"
       [ arc cVar (armRes "bonusTo") oVar ]
       [ arc cVar (armRes "traitClass") (armRes "Bonus")
       , arc cVar typeRes  (armRes "Bonus") ]

classRules  :: [RDFRule]
classRules = [ bonusclassrule, traitclasstypeRule
             , advclasstypeRule, charclasstypeRule ]
