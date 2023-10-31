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
module ArM.Markdown.SheetObject ( getSheetObject
                                , SheetObject(..)
                                , Trait(..)
                                , tefoString
                                ) where

import qualified Swish.RDF.Query as Q
import qualified Swish.RDF.Graph as G
import           ArM.CharacterQuery 
import           ArM.KeyPair 
import           ArM.Resources 
import           ArM.Rules.Aux 
import Data.List(sortOn)

import Data.Maybe (fromJust)
import Swish.VarBinding  (vbMap)

getCharTraits :: G.RDFGraph -> [Trait]
getCharTraits = sortOn traitOrder . (map parseTrait) . getCharacteristics
getPTraits :: G.RDFGraph -> [Trait]
getPTraits = sortOn traitOrder . (map parseTrait) . getPTs

getAbilityTraits :: G.RDFGraph -> [Trait]
getAbilityTraits = (map parseTrait) . getAbilities

-- getCombat :: G.RDFGraph -> [Trait]
-- getCombat = getTraitList $ armRes "hasCombatOption"

getSheetObject :: G.RDFGraph -> SheetObject
getSheetObject g = SheetObject {
    metadata = mdSort $ getMetaDataTuples g,
    abilities = getAbilityTraits g,
    arts = map parseTrait $ getArts g,
    spells = map parseTrait $ getSpells g,
    characteristics = getCharTraits g,
    ptraits = getPTraits g,
    virtues = map parseTrait $ getVirtues g,
    flaws = map parseTrait $ getFlaws g,
    size = getSize g,
    cnf = getConf g
}

data SheetObject = SheetObject {
    metadata :: [(String,String)],
    abilities :: [Trait],
    arts :: [Trait],
    spells :: [Trait],
    characteristics :: [Trait],
    ptraits :: [Trait],
    virtues :: [Trait],
    flaws :: [Trait],
    size :: [Maybe Int],
    cnf :: [(Maybe Int,Maybe Int)]
}


data Trait = Trait {
  traitLabel :: Maybe String,
  traitAbbr :: Maybe String,
  traitSpeciality :: Maybe String,
  traitXP :: Maybe Int,
  traitScore :: Maybe Int,
  traitDetail :: Maybe String,
  traitCastingScore :: Maybe Int,
  traitForm :: Maybe String,
  traitTech :: Maybe String,
  traitLevel :: Maybe Int,
  traitOrder :: Maybe Int
}
defaultTrait :: Trait
defaultTrait = Trait {
  traitLabel = Nothing,
  traitAbbr = Nothing,
  traitSpeciality = Nothing,
  traitXP = Nothing,
  traitScore = Nothing,
  traitDetail = Nothing,
  traitCastingScore = Nothing,
  traitForm = Nothing,
  traitTech = Nothing,
  traitLevel = Nothing,
  traitOrder = Nothing
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




parsePair :: KeyValuePair -> Trait -> Trait
parsePair (KeyValuePair res  x) t 
    | res == (armRes "hasLabel")      = t { traitLabel = G.fromRDFLabel x }
    | res == (armRes "hasSpeciality") = t { traitSpeciality = G.fromRDFLabel x }
    | res == (armRes "hasScore")      = t { traitScore = G.fromRDFLabel x }
    | res == (armRes "hasXP")         = t { traitXP = G.fromRDFLabel x }
    | res == (armRes "hasDetail")     = t { traitDetail = G.fromRDFLabel x }
    | res == (armRes "hasCastingScore") = t { traitCastingScore = G.fromRDFLabel x }
    | res == (armRes "hasFormString")   = t { traitForm = G.fromRDFLabel x }
    | res == (armRes "hasTechniqueString") = t { traitTech = G.fromRDFLabel x }
    | res == (armRes "hasLevel")           = t { traitLevel = G.fromRDFLabel x }
    | res == (armRes "hasOrder")           = t { traitOrder = G.fromRDFLabel x }
    | res == (armRes "hasAbbreviation")    = t { traitAbbr = G.fromRDFLabel x }
parsePair _ t = t 

tefoString :: Trait -> String
tefoString t = tt (traitTech t) ++ tt (traitForm t) ++ (fJi $ traitLevel t)
   where tt = take 2 . fJ
         fJ Nothing = "Xx"
         fJ (Just x) = x
         fJi Nothing = "X"
         fJi (Just x) = show x
         
mdSort :: [(String, b)] -> [(String, b)]
mdSort = sortOn mds . filter (\ x -> mds x > 0)
   where mds = mdSortKey . fst

mdSortKey :: String -> Int
mdSortKey "Name" = 10
mdSortKey "Season" = 11
mdSortKey "Year" = 12
mdSortKey "Player" = 20
mdSortKey "Birth Year" = 30
mdSortKey "Age" = 40
mdSortKey "Gender" = 50
mdSortKey "Covenant" = 60
mdSortKey "Alma Mater" = 70
mdSortKey "Nationality" = 80
mdSortKey "Character Type" = 0
mdSortKey _ = 2^(30 :: Int)

{-
 - Spells
 - KeyValuePair arm:hasTargetString "Individual",
 - KeyValuePair arm:hasRangeString "Touch",
 - KeyValuePair arm:hasDurationString "Sun",
 - KeyValuePair arm:hasDescription "The target becomes completely undetectable to normal sight, regardless of what he does, but still casts a shadow.  (Base 4, +1 Touch, +2 Sun, +1 changing image)",KeyValuePair arm:hasCastingScore 19]
-}
