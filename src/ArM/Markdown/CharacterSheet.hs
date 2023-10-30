{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.CharGen
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle characters as stored in web server memory.
--
-----------------------------------------------------------------------------
module ArM.Markdown.CharacterSheet where

import Swish.RDF.Graph as G
import ArM.KeyPair()
-- import ArM.Types.Character
-- import ArM.Types.Season
-- import ArM.Types.Advancement

import           ArM.KeyPair
import           ArM.TraitQuery
import           ArM.CharacterQuery
import Data.List(sortOn,intercalate)


printVirtues :: RDFGraph -> [String]
printVirtues = map printVFLine . getVirtueTraits

printFlaws :: RDFGraph -> [String]
printFlaws = map printVFLine. getFlawTraits

printVF :: RDFGraph -> [String]
printVF g = "## Virtues and Flaws":"":(printVirtues g ++ printFlaws g)


printVFLine :: Trait -> String
printVFLine t = "+ " ++ f1 t ++ f3 t ++ " (" ++ f2 t ++ ")"
   where vfDetail Nothing = "" 
         vfDetail (Just s) = ": " ++ s
         f3 = vfDetail . traitDetail
         f1 = ss . traitLabel
         f2 = si . traitScore


printArts :: RDFGraph -> [String]
printArts = printArts' . getArtTraits
printArts' :: [Trait] -> [String]
printArts' = ("## Arts":) . ("":) .
             ("| Art\t | Score\t | XP |":) .
             ("| :- \t |    -:\t | -: |":) .
             map printArtLine

printArtLine :: Trait -> String
printArtLine t = "| " ++ (ss $ traitLabel t) ++ "\t | " 
                         ++ (si $ traitScore t) ++ "\t| "
                         ++ (si $ traitXP  t) ++ "\t|"
printAbilityLine :: Trait -> String
printAbilityLine t = "| " ++ (ss $ traitLabel t) ++ "\t | " 
                         ++ (ss $ traitSpeciality t) ++ "\t | "
                         ++ (si $ traitScore t) ++ "\t| "
                         ++ (si $ traitXP  t) ++ "\t|"


printAbilities :: RDFGraph -> [String]
printAbilities = printAbilities' . getAbilityTraits
printAbilities' :: [Trait] -> [String]
printAbilities' = ("## Abilities":) . ("":) .
             ("| Ability\t | Speciality\t | Score\t| XP\t|":) .
             ("| :-     \t | :-        \t |   -: \t| -:\t|":) .
             map printAbilityLine

printSpells :: RDFGraph -> [String]
printSpells t = "## Spells":"":f t
   where f = map printSpellLine . getSpellTraits
printSpellLine :: Trait -> String
printSpellLine t = "+ " ++ f1 t ++ f3 t ++ " *Casting Score* " ++ f2 t ++ 
      "; *Mastery* "
      ++ f4 t ++ " (" ++ f5 t ++ ")"
   where vfDetail Nothing = "" 
         vfDetail (Just s) = " [" ++ s ++ "]"
         f3 = vfDetail . traitDetail
         f1 = ss . traitLabel
         f4 = si . traitScore
         f5 = si . traitXP
         f2 = si . traitCastingScore

printMetaData :: RDFGraph -> [String]
printMetaData = (map printMD ) . mdSort . getMetaDataTuples
printMD :: (String,String) -> String
printMD (x, y) = x ++ "\n: " ++ y

printMisc :: RDFGraph -> [String]
printMisc g = [ "Size", ": " ++ lf size, "Confidence", ": " ++ lf cnf  ]
    where size = map maybeFormat $ getSize g
          cnf = map confFormat $ getConf g
          lf [] = "-"
          lf (x:[]) = x
          lf xs = intercalate ", " xs
confFormat :: (Maybe Int, Maybe Int) -> String
confFormat (x,y) = maybeFormat x ++ " (" ++ maybeFormat y ++ ")"
maybeFormat :: (Show a) => Maybe a -> String
maybeFormat Nothing = "-"
maybeFormat (Just x) = show x


tuttishow :: KeyPairList -> String
tuttishow (KeyPairList ls) = show ls
-- printArtLine :: KeyPairList -> String

-- Debug
debugSpells :: RDFGraph -> [String]
debugSpells = map tuttishow . getSpells

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

ss :: Maybe String -> String
ss (Just s) = s
ss Nothing = "-"
si :: Maybe Int -> String
si (Just s) = show s
si Nothing = "-"
