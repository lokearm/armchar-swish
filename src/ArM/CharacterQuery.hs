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
                          , getCombat
                          ) where

import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import           ArM.Resources 
import           ArM.KeyPair
import qualified ArM.Character.Trait as CT
import ArM.Rules.Aux

arcs :: G.RDFLabel -> G.RDFGraph
arcs prop = listToRDFGraph [ G.arc sVar typeRes csRes
           , G.arc sVar prop idVar 
           , G.arc idVar propertyVar valueVar 
           , G.arc propertyVar labelRes labelVar  ]


oldgetTraitList :: G.RDFLabel -> G.RDFGraph -> [CT.Trait]
oldgetTraitList prop = map toTrait 
               . arcListSplit . map arcFromBinding . Q.rdfQueryFind q
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
getCombat :: G.RDFGraph -> [KeyPairList]
getCombat = getTraitList $ armRes "hasCombatOption"

getItemList :: G.RDFGraph -> [CT.Item]
getItemList = map toItem 
               . arcListSplit . map arcFromBinding . Q.rdfQueryFind q
    where q = arcs $ armRes "hasPossession"
          toItem = CT.kpToItem . KeyPairList . toKeyPairList 

traitarcs :: G.RDFLabel -> G.RDFGraph
traitarcs p = listToRDFGraph 
   [ G.arc cVar p idVar                                -- sheet has trait
   -- , G.arc propertyVar typeRes (armRes "ViewProperty") -- property of interest
   , G.arc idVar propertyVar valueVar                  -- triple of interest
   , G.arc propertyVar labelRes labelVar ]             -- property label
getTraitList :: G.RDFLabel -> G.RDFGraph -> [KeyPairList]
getTraitList p = map ( KeyPairList . toKeyPairList ) . arcListSplit . map arcFromBinding . Q.rdfQueryFind (traitarcs p)

