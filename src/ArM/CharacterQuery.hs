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
import qualified ArM.Character.Character as C
import           ArM.Resources 
import           ArM.KeyPair
import qualified ArM.Character.Trait as CT
import Data.Maybe (fromJust)
import Data.Set (fromList)
import ArM.Rules.Aux
import Swish.RDF.Vocabulary.RDF

idVar = (G.Var "id")
propertyVar = (G.Var "property")
valueVar = (G.Var "value")
labelVar = (G.Var "label")
arcs :: G.RDFLabel -> G.RDFGraph
arcs prop = G.toRDFGraph . fromList $ [ G.arc sVar typeRes csRes
           , G.arc sVar prop idVar 
           , G.arc idVar propertyVar valueVar 
           , G.arc propertyVar labelRes labelVar  ]


getTraitList :: G.RDFLabel -> G.RDFGraph -> [CT.Trait]
getTraitList prop = map CT.toTrait 
               . keypairSplit . map objectFromBinding . Q.rdfQueryFind q
    where q = arcs prop

getVirtues = getTraitList $ G.Res $ makeSN "hasVirtue"
getFlaws = getTraitList $ G.Res $ makeSN "hasFlaw"
getPTs = getTraitList $ G.Res $ makeSN "hasPersonalityTrait"
getAbilities = getTraitList $ G.Res $ makeSN "hasAbility"
getArts = getTraitList $ G.Res $ makeSN "hasArt"
getReputations = getTraitList $ G.Res $ makeSN "hasReputation"
getSpells = getTraitList $ G.Res $ makeSN "hasSpell"
getCharacteristics = getTraitList $ G.Res $ makeSN "hasCharacteristic"
