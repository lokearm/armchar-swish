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
-- Only queries on the character sheet are defined in this module.
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
                          , getCombat
                          , getMetaData
                          ) where

import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import           ArM.Resources 
import           ArM.KeyPair
import ArM.Rules.Aux

getVirtues :: G.RDFGraph -> [KeyPairList]
getVirtues = getTraitList $ armRes "hasVirtue"
getFlaws :: G.RDFGraph -> [KeyPairList]
getFlaws = getTraitList $ armRes "hasFlaw"
getPTs :: G.RDFGraph -> [KeyPairList]
getPTs = getTraitList $ armRes "hasPersonalityTrait"
getAbilities :: G.RDFGraph -> [KeyPairList]
getAbilities = getTraitList $ armRes "hasAbility"
getArts :: G.RDFGraph -> [KeyPairList]
getArts = getTraitList $ armRes "hasArt"
getReputations :: G.RDFGraph -> [KeyPairList]
getReputations = getTraitList $ armRes "hasReputation"
getSpells :: G.RDFGraph -> [KeyPairList]
getSpells = getTraitList $ armRes "hasSpell"
getCharacteristics :: G.RDFGraph -> [KeyPairList]
getCharacteristics = getTraitList $ armRes "hasCharacteristic"
getItemList :: G.RDFGraph -> [KeyPairList]
getItemList = getTraitList $ armRes "hasPossession"

getCombat :: G.RDFGraph -> [KeyPairList]
getCombat = getTraitList $ armRes "hasCombatOption"

traitarcs :: G.RDFLabel -> G.RDFGraph
traitarcs p = listToRDFGraph 
   [ G.arc cVar p idVar                                -- sheet has trait
   -- , G.arc propertyVar typeRes (armRes "ViewProperty") -- property of interest
   , G.arc idVar propertyVar valueVar                  -- triple of interest
   , G.arc propertyVar labelRes labelVar ]             -- property label
getTraitList :: G.RDFLabel -> G.RDFGraph -> [KeyPairList]
getTraitList p = map ( KeyPairList . toKeyPairList ) . arcListSplit 
               . map arcFromBinding . Q.rdfQueryFind (traitarcs p)


arcs :: G.RDFGraph
arcs = listToRDFGraph 
   [ G.arc idVar typeRes csRes                          -- type CharacterSheet
   , G.arc propertyVar typeRes (armRes "ViewProperty") -- property of interest
   , G.arc idVar propertyVar valueVar                   -- triple of interest
   , G.arc propertyVar labelRes labelVar ]             -- property label


-- | The query function `getMetaData` returns all triples with an
-- arm:ViewProperty.  There is no error checking.
-- The given graph must contain one CharacterSheet only.
getMetaData :: G.RDFGraph -> KeyPairList
getMetaData = KeyPairList . toKeyPairList . map arcFromBinding . Q.rdfQueryFind arcs
