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
-- Character Sheet.  The input for all the functions is a Character Record
-- as stored in `CharacterMap`.  
-- 
--
-----------------------------------------------------------------------------
module ArM.CharacterQuery ( getTraitList
                          , getVirtues
                          , getFlaws
                          , getPTs
                          , getAbilities
                          , getArts
                          , getReputations
                          , getSpells
                          , getCharacteristics
                          , getItemList
                          ) where

import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import qualified ArM.Character.Character as C
import           ArM.Resources 
import           ArM.KeyPair
import qualified ArM.Character.Trait as CT
import ArM.Rules.Aux
import Swish.RDF.Vocabulary.RDF

arcs :: G.RDFLabel -> G.RDFGraph
arcs prop = listToRDFGraph [ G.arc sVar typeRes csRes
           , G.arc sVar prop idVar 
           , G.arc idVar propertyVar valueVar 
           , G.arc propertyVar labelRes labelVar  ]


getTraitList :: G.RDFLabel -> G.RDFGraph -> [CT.Trait]
getTraitList prop = map toTrait 
               . keypairSplit . map objectFromBinding . Q.rdfQueryFind q
    where q = arcs prop
          toTrait = CT.kpToTrait . toKeyPairList 

getVirtues = getTraitList $ armRes "hasVirtue"
getFlaws = getTraitList $ armRes "hasFlaw"
getPTs = getTraitList $ armRes "hasPersonalityTrait"
getAbilities = getTraitList $ armRes "hasAbility"
getArts = getTraitList $ armRes "hasArt"
getReputations = getTraitList $ armRes "hasReputation"
getSpells = getTraitList $ armRes "hasSpell"
getCharacteristics = getTraitList $ armRes "hasCharacteristic"

getItemList :: G.RDFGraph -> [CT.Item]
getItemList = map toItem 
               . keypairSplit . map objectFromBinding . Q.rdfQueryFind q
    where q = arcs $ armRes "hasPossession"
          toItem = CT.kpToItem . KeyPairList . toKeyPairList 
