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
module ArM.TraitQuery where

import qualified Swish.RDF.Query as Q
import qualified Swish.RDF.Graph as G
import           ArM.CharacterQuery 
import           ArM.KeyPair 
import           ArM.Resources 
import           ArM.Rules.Aux 

import Data.Maybe (fromJust)
import Swish.VarBinding  (vbMap)

getVirtueTraits :: G.RDFGraph -> [Trait]
getVirtueTraits = (map parseTrait) . getVirtues
getFlawTraits :: G.RDFGraph -> [Trait]
getFlawTraits = (map parseTrait) . getFlaws
getAbilityTraits :: G.RDFGraph -> [Trait]
getAbilityTraits = (map parseTrait) . getAbilities
getArtTraits :: G.RDFGraph -> [Trait]
getArtTraits = (map parseTrait) . getArts

getSpellTraits :: G.RDFGraph -> [Trait]
getSpellTraits = (map parseTrait) . getSpells

-- getCombat :: G.RDFGraph -> [Trait]
-- getCombat = getTraitList $ armRes "hasCombatOption"


data Trait = Trait {
  traitLabel :: Maybe String,
  traitSpeciality :: Maybe String,
  traitXP :: Maybe Int,
  traitScore :: Maybe Int,
  traitDetail :: Maybe String,
  traitCastingScore :: Maybe Int,
  traitForm :: Maybe String,
  traitTech :: Maybe String,
  traitLevel :: Maybe Int
}
defaultTrait :: Trait
defaultTrait = Trait {
  traitLabel = Nothing,
  traitSpeciality = Nothing,
  traitXP = Nothing,
  traitScore = Nothing,
  traitDetail = Nothing,
  traitCastingScore = Nothing,
  traitForm = Nothing,
  traitTech = Nothing,
  traitLevel = Nothing
}


parseTrait :: KeyPairList -> Trait
parseTrait (KeyPairList pl) = parseTrait' pl defaultTrait 
parseTrait' :: [KeyValuePair] -> Trait -> Trait
parseTrait' [] t = t
parseTrait' (x:xs) t = parseTrait' xs (parsePair x t)

getSize :: G.RDFGraph -> [Maybe Int]
getSize g = map v find 
       where find = Q.rdfQueryFind sizeGraph g
             f = G.fromRDFLabel . fromJust 
             v x = f $ vbMap x valueVar
getConf :: G.RDFGraph -> [(Maybe Int,Maybe Int)]
getConf g = map v find
       where find = Q.rdfQueryFind confGraph g
             f = G.fromRDFLabel . fromJust 
             v x = (f $ vbMap x valueVar,f $ vbMap x pVar)

sizeGraph :: G.RDFGraph
sizeGraph = listToRDFGraph [ G.arc idVar (armRes "hasTrait" ) tVar 
                           , G.arc tVar (typeRes) (armRes "Size")
                           , G.arc tVar (armRes "hasScore" ) valueVar
                           ]
confGraph :: G.RDFGraph
confGraph = listToRDFGraph [ G.arc idVar (armRes "hasTrait" ) tVar 
                           , G.arc tVar (typeRes) (armRes "Confidence")
                           , G.arc tVar (armRes "hasScore" ) valueVar
                           , G.arc tVar (armRes "hasPoints" ) pVar
                           ]

labRes :: G.RDFLabel
labRes = armRes "hasLabel"
specRes  :: G.RDFLabel
specRes  = armRes "hasSpeciality"
scoreRes :: G.RDFLabel
scoreRes = armRes "hasScore"
xpRes    :: G.RDFLabel
xpRes    = armRes "hasXP"
castingRes   :: G.RDFLabel
castingRes   = armRes "hasCastingScore"
detailRes    :: G.RDFLabel
detailRes    = armRes "hasDetail"

parsePair :: KeyValuePair -> Trait -> Trait
parsePair (KeyValuePair res  x) t 
    | res == labRes   = t { traitLabel = G.fromRDFLabel x }
    | res == specRes  = t { traitSpeciality = G.fromRDFLabel x }
    | res == scoreRes = t { traitScore = G.fromRDFLabel x }
    | res == xpRes    = t { traitXP = G.fromRDFLabel x }
    | res == detailRes    = t { traitDetail = G.fromRDFLabel x }
    | res == castingRes   = t { traitCastingScore = G.fromRDFLabel x }
    | res == (armRes "hasFormString")   = t { traitForm = G.fromRDFLabel x }
    | res == (armRes "hasTechniqueString")   = t { traitTech = G.fromRDFLabel x }
    | res == (armRes "hasLevel")   = t { traitLevel = G.fromRDFLabel x }
parsePair _ t = t 

tefoString :: Trait -> String
tefoString t = tt (traitTech t) ++ tt (traitForm t) ++ (fJi $ traitLevel t)
   where tt = take 2 . fJ
         fJ Nothing = "Xx"
         fJ (Just x) = x
         fJi Nothing = "X"
         fJi (Just x) = show x
         

{-
 - Spells
 - KeyValuePair arm:hasTargetString "Individual",
 - KeyValuePair arm:hasRangeString "Touch",
 - KeyValuePair arm:hasDurationString "Sun",
 - KeyValuePair arm:hasDescription "The target becomes completely undetectable to normal sight, regardless of what he does, but still casts a shadow.  (Base 4, +1 Touch, +2 Sun, +1 changing image)",KeyValuePair arm:hasCastingScore 19]
-}
