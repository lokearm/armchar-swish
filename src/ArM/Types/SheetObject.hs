{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.SheetObject
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
--
-----------------------------------------------------------------------------
module ArM.Types.SheetObject ( getSheetObject
                                , SheetObject(..)
                                , Trait(..)
                                , traitLabel
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


sortparsed :: [KeyPairList] -> [Trait] 
sortparsed = sortOn traitOrder . (map parseTrait) 

getAbilityTraits :: G.RDFGraph -> [Trait]
getAbilityTraits = (map parseTrait) . getAbilities

getSheetObject :: G.RDFGraph -> SheetObject
getSheetObject g = SheetObject {
    metadata = mdSort $ getMetaDataTuples g,
    abilities = getAbilityTraits g,
    arts = sortparsed $ getArts g,
    spells = map parseTrait $ getSpells g,
    characteristics = sortparsed $ getCharacteristics g,
    ptraits = sortparsed $ getPTs g,
    virtues = map parseTrait $ getVirtues g,
    flaws = map parseTrait $ getFlaws g,
    combat = map parseTrait $ getCombat g,
    equipment = map parseTrait $ getEquipment g,
    vis = map parseTrait $ getVis g,
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
    combat :: [Trait],
    equipment :: [Trait],
    vis :: [Trait],
    size :: [Maybe Int],
    cnf :: [(Maybe Int,Maybe Int)]
}


traitLabel :: Trait -> Maybe String
traitLabel  t
  | traitLabel2 t == Nothing = traitLabel1 t
  | traitLabel2 t == (Just "") = traitLabel1 t
  | otherwise = traitLabel2 t

data Trait = Trait {
  traitLabel1 :: Maybe String,
  traitLabel2 :: Maybe String,
  traitAbbr :: Maybe String,
  traitSpeciality :: Maybe String,
  traitXP :: Maybe Int,
  traitScore :: Maybe Int,
  traitTotalScore :: Maybe Int,
  traitDetail :: Maybe String,
  traitCastingScore :: Maybe Int,
  traitForm :: Maybe String,
  traitTech :: Maybe String,
  traitLevel :: Maybe Int,
  traitOrder :: Maybe Int,
  traitBonus :: Maybe Int,
  traitInit :: Maybe Int,
  traitAtk :: Maybe Int,
  traitDfn :: Maybe Int,
  traitDam :: Maybe Int,
  traitStr :: Maybe Int,
  traitRange :: Maybe Int,
  traitLoad :: Maybe Int,
  traitCost :: Maybe String
}
defaultTrait :: Trait
defaultTrait = Trait {
  traitLabel1 = Nothing,
  traitLabel2 = Nothing,
  traitAbbr = Nothing,
  traitSpeciality = Nothing,
  traitXP = Nothing,
  traitScore = Nothing,
  traitTotalScore = Nothing,
  traitDetail = Nothing,
  traitCastingScore = Nothing,
  traitForm = Nothing,
  traitTech = Nothing,
  traitLevel = Nothing,
  traitOrder = Nothing,
  traitBonus = Nothing,
  traitInit = Nothing,
  traitAtk = Nothing,
  traitDfn = Nothing,
  traitDam = Nothing,
  traitRange = Nothing,
  traitStr = Nothing,
  traitLoad = Nothing,
  traitCost = Nothing
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
    | res == (armRes "hasLabel")      = t { traitLabel1 = G.fromRDFLabel x }
    | res == (armRes "instanceLabel") = t { traitLabel2 = G.fromRDFLabel x }
    | res == (armRes "hasSpeciality") = t { traitSpeciality = G.fromRDFLabel x }
    | res == (armRes "hasScore")      = t { traitTotalScore = G.fromRDFLabel x }
    | res == (armRes "hasXPScore")      = t { traitScore = G.fromRDFLabel x }
    | res == (armRes "hasXP")         = t { traitXP = G.fromRDFLabel x }
    | res == (armRes "hasDetail")     = t { traitDetail = G.fromRDFLabel x }
    | res == (armRes "hasCastingScore") = t { traitCastingScore = G.fromRDFLabel x }
    | res == (armRes "hasFormString")   = t { traitForm = G.fromRDFLabel x }
    | res == (armRes "hasTechniqueString") = t { traitTech = G.fromRDFLabel x }
    | res == (armRes "hasLevel")           = t { traitLevel = G.fromRDFLabel x }
    | res == (armRes "hasOrder")           = t { traitOrder = G.fromRDFLabel x }
    | res == (armRes "hasAbbreviation")    = t { traitAbbr = G.fromRDFLabel x }
    | res == (armRes "hasTotalBonus")      = t { traitBonus = G.fromRDFLabel x }
    | res == (armRes "hasInit")      = t { traitInit = G.fromRDFLabel x }
    | res == (armRes "hasAtk")      = t { traitAtk = G.fromRDFLabel x }
    | res == (armRes "hasDfn")      = t { traitDfn = G.fromRDFLabel x }
    | res == (armRes "hasDam")      = t { traitDam = G.fromRDFLabel x }
    | res == (armRes "hasWeaponRange")      = t { traitRange = G.fromRDFLabel x }
    | res == (armRes "hasLoad")      = t { traitLoad = G.fromRDFLabel x }
    | res == (armRes "hasStr")      = t { traitStr = G.fromRDFLabel x }
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
