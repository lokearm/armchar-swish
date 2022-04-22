-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Rules.Resources
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Reasoning rules using the resource ontology.
--
-----------------------------------------------------------------------------

module ArM.Rules.Resource where

import Swish.RDF.Ruleset
import qualified Data.Text as T
import Swish.Rule
import Swish.RDF.Graph
import Swish.RDF.Vocabulary.RDF
import ArM.Resources
import ArM.Rules.Aux
import Swish.VarBinding (varBindingId) 
import Data.Set

-- | Infere resource properties from class
prepareGraph = fwdApplyList [ rRule ]

-- | Deprecated.  This is a version of 'rRule' using N3 notation
-- and full URIs
rRule' = makeRule' "rRule" 
  ( prefixes 
  ++ "?s <https://hg.schaathun.net/armchar/schema#traitClass> ?t . "
  ++ "?t ?p ?o . "
  ++ "?p rdf:type <https://hg.schaathun.net/armchar/schema#TraitProperty> ."
  )
  "?s ?p ?o ."

rRule = makeRDFClosureRule ( makeSN "rRule" )
            [listToRDFGraph  l1]
            (listToRDFGraph  l2)
            varBindingId
    where s = (Var "s")
          t = (Var "t")
          o = (Var "o")
          p = (Var "p")
          l1 = [ arc s ( Res $ makeSN "traitClass" ) t,
               arc t p o,
               arc p ( Res rdfType ) ( Res $ makeSN "TraitProperty" )  ]
          l2 = [arc s p o]

listToRDFGraph :: [RDFTriple] -> RDFGraph
listToRDFGraph = toRDFGraph .  fromList 
