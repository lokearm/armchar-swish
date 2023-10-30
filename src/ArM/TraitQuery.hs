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
  traitCastingScore :: Maybe Int
}
defaultTrait :: Trait
defaultTrait = Trait {
  traitLabel = Nothing,
  traitSpeciality = Nothing,
  traitXP = Nothing,
  traitScore = Nothing,
  traitDetail = Nothing,
  traitCastingScore = Nothing
}


parseTrait :: KeyPairList -> Trait
parseTrait (KeyPairList pl) = parseTrait' pl defaultTrait 
parseTrait' :: [KeyValuePair] -> Trait -> Trait
parseTrait' [] t = t
parseTrait' (x:xs) t = parseTrait' xs (parsePair x t)

getSize :: G.RDFGraph -> [Maybe Int]
getSize g = map value (find g)
       where find = Q.rdfQueryFind $ listToRDFGraph [ G.arc idVar (armRes "hasSize") valueVar ]
             value f = G.fromRDFLabel $ fromJust $ vbMap f (G.Var "value")
getConf :: G.RDFGraph -> [(Maybe Int,Maybe Int)]
getConf g = map v find
       where find = Q.rdfQueryFind confGraph g
             f = G.fromRDFLabel . fromJust 
	     v x = (f $ vbMap x valueVar,f $ vbMap x pVar)
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
parsePair _ t = t 

