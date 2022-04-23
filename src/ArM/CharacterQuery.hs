{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.CharacterQuery
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Queries to build JSON files describing separate blocks of the
-- Character Sheet.  The input for all the functions is a CharacterRecord
-- as stored in `CharacterMap`.  
-- 
--
-----------------------------------------------------------------------------
module ArM.CharacterQuery where

import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import qualified ArM.Character as C
import           ArM.Resources 
import           ArM.Query 
import qualified ArM.Internal.Trait as IT
import Data.Maybe (fromJust)
import Data.Set (fromList)
import ArM.Rules.Aux
import Swish.RDF.Vocabulary.RDF

cqt :: G.RDFLabel -> G.RDFGraph
cqt s = qparse $ prefixes 
   ++ "?s rdf:type <https://hg.schaathun.net/armchar/schema#CharacterSheet> . "
   ++ "?s <https://hg.schaathun.net/armchar/schema#hasTrait> ?id . " 
   ++ "?id ?property ?value . "
   ++ "?property rdfs:label ?label . "

idVar = (G.Var "id")
propertyVar = (G.Var "property")
valueVar = (G.Var "value")
labelVar = (G.Var "label")
arcs :: G.RDFLabel -> G.RDFGraph
arcs prop = G.toRDFGraph . fromList $ [ G.arc sVar typeRes csRes
           , G.arc sVar prop idVar 
           , G.arc idVar propertyVar valueVar 
           , G.arc propertyVar labelRes labelVar  ]


getTraitList :: G.RDFLabel -> G.RDFGraph -> [IT.Trait]
getTraitList prop = map IT.toTrait 
               . quadSplit . map quadFromBinding . Q.rdfQueryFind q
    where q = arcs prop

getVirtues = getTraitList $ G.Res $ makeSN "hasVirtue"
getFlaws = getTraitList $ G.Res $ makeSN "hasFlaw"
getPTs = getTraitList $ G.Res $ makeSN "hasPersonalityTrait"
getAbilities = getTraitList $ G.Res $ makeSN "hasAbility"
getArts = getTraitList $ G.Res $ makeSN "hasArt"
getReputations = getTraitList $ G.Res $ makeSN "hasReputation"
getSpells = getTraitList $ G.Res $ makeSN "hasSpell"
