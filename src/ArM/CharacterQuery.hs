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
                          , getPossessions
                          , getVis
                          , getEquipment
                          , getCombat
                          , getMetaData
                          , getMetaDataTuples
                          ) where

import Data.Maybe (fromJust)

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
getPossessions :: G.RDFGraph -> [KeyPairList]
getPossessions = getTraitList $ armRes "hasPossession"
getEquipment :: G.RDFGraph -> [KeyPairList]
getEquipment = getTraitList $ armRes "hasEquipment"
getVis :: G.RDFGraph -> [KeyPairList]
getVis = getTraitList $ armRes "hasVis"

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

getMetaDataTuples :: G.RDFGraph -> [(String, String)]
getMetaDataTuples = map ( mdPair . metadataFromBinding ) . Q.rdfQueryFind arcs

mdPair :: (a, Maybe G.RDFLabel, Maybe G.RDFLabel) -> (String,String)
mdPair (_,x,y) = (label2string x, label2string y)
fromRDFLabel :: G.RDFLabel -> (Maybe Int, Maybe String)
fromRDFLabel lab = (G.fromRDFLabel lab,G.fromRDFLabel lab)
label2string :: Maybe G.RDFLabel -> String
label2string = f . fromRDFLabel . fromJust
    where f (Nothing,Nothing) = "Error"
          f (Just x,Nothing) = show x
          f (Nothing,Just x) = x
          f _ = error "label2string: Fallthrough case should not happen"
