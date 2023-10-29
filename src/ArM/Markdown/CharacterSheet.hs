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

-- |
-- = Data Types

-- ^ A `CharStage` object represents a character's state of development
-- at one particular point on the in-game timeline. 
-- data CharStage = CharStage 
-- { advancement :: Advancement  ^ The advancement leading to the stage
-- , sheetObject :: CharacterSheet     ^ The resulting character sheet
-- , sheetGraph :: RDFGraph ^ The character sheet as an RDF Graph
--      }  deriving (Eq,Show)

-- data CharacterSheet = CharacterSheet {
-- csID :: RDFLabel, ^ Character ID (i.e. same ID for every season)
-- sheetID :: Maybe RDFLabel,  ^ ID of the Character Sheet, usually Nothing suggesting a blank node
-- csTime :: CharTime,  -- ^ Current Year
-- born     :: Int,      -- ^ Year of Birth
-- csItems :: [Trait],    -- ^ List of possessions (weapons, equipment)
-- csTraits :: [Trait],  -- ^ List of traits (abilities, spells, etc.)
-- csMetadata :: KeyPairList ^ Metadata, i.e. data which are not traits or items.
-- }  deriving (Eq)


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

printMetaData :: RDFGraph -> [String]
printMetaData = printMetaData' . getMetaData
printMetaData' :: KeyPairList -> [String]
printMetaData' x = [tuttishow x]

tuttishow :: KeyPairList -> String
tuttishow (KeyPairList ls) = show ls
-- printArtLine :: KeyPairList -> String

-- Debug
-- debugArts :: RDFGraph -> [String]
-- debugArts = map tuttishow . getArts
