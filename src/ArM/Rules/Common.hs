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
import ArM.Resources
import ArM.Rules.Aux

import Control.Parallel

-- | Infer rdf:type from arm:traitClass
traitclasstypeRule = makeCRule "traitclasstypeRule" 
       [ arc sVar ( armRes "traitClass" ) tVar ]
       [ arc sVar typeRes tVar ]

-- | Many Object Properties are used only internally, and have an
-- associate property of string type for display purposes.  These
-- string properties are inferred by this rule.
stringPropertyRule = makeCRule "stringRule1"
       [ arc cVar pVar oVar
       , arc pVar (armRes "hasStringProperty") ( Var "p2" )
       , arc oVar (armRes "hasLabel") sVar ]
       [ arc cVar (Var "p2") sVar ]
