{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Combat
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Character Traits, including Abilities, Spells, Virtues, etc..
--
-- This module proves a type for each kind of trait as well as a wrapper type,
-- `Trait` which can represent any kind of trait.
--
-- This module defines the types as well as the `TraitType` class, and instances
-- for `show`, sorting, and JSON.
--
-----------------------------------------------------------------------------------
module ArM.Char.Combat where

-- import ArM.Helper
import ArM.Char.CharacterSheet
import ArM.Char.Types.Trait
import ArM.Char.Types.TraitKey
import ArM.DB.Weapon
import ArM.Helper

import GHC.Generics
import Data.Maybe
import Data.List
import qualified Data.Map as M

-- |
-- = The Trait Type

data CombatLine = CombatLine 
    { combatLabel :: String
    , combatInit  :: Int
    , combatAtk   :: Maybe Int
    , combatDef   :: Maybe Int
    , combatDam   :: Maybe Int
    , combatRange :: Maybe Int
    , combatLoad  ::  Int
    , combatComment :: String
    } deriving (Show, Eq, Generic)
defaultCL :: CombatLine 
defaultCL = CombatLine 
    { combatLabel = "No label"
    , combatInit  = 0
    , combatAtk   = Nothing
    , combatDef   = Nothing
    , combatDam   = Nothing
    , combatRange = Nothing
    , combatLoad  = 0
    , combatComment = ""
    } 
data SoakLine = SoakLine 
    { soakLabel    :: String
    , soakScore    :: Int
    , soakLoad     ::  Int
    } deriving (Show, Eq, Generic)


-- | Look up weapon stats for a possession.
-- Standard stats from the DB and specific stats from the item are concatenated.
weaponStat :: WeaponDB -> Possession -> Maybe Weapon
weaponStat db p = ws
   where ws' = map (\ x -> M.lookup x db) $ weapon p
         ws = weaponStats p ++ filterNothing ws'

-- : A dummy combat line with error message
noLine :: CombatLine
noLine = defaultCL
        { combatLabel = combatName co
        , combatComment = "No weapon"
        }


-- | Compute a single line for the Combat Stats table
computeCombatLine :: WeaponDB -> CharacterSheet -> CombatOption -> CombatLine
computeCombatLine db cs co | isNothing ab = addCharacteristics $ implicitAbility db cs co
                           | otherwise    = addCharacteristics $ explicitAbility db cs co (fromJust ab)
     where ab = combatAbikity co

-- | Add characteristics to the combat stats
addCharacteristics :: CharacterSheet -> CombatLine -> CombatLine
addCharacteristics cs cl = cl
        { combatInit  = combatInit cl + qik
        , combatAtk   = combatAtl cl + dex
        , combatDef   = combatDef cl + qik 
        , combatDam   = combatDam cl + str 
        } 
   where qik = sheetCharacteristicScore cs (CharacteristicKey "qik")
	 dex = sheetCharacteristicScore cs (CharacteristicKey "dex")
	 str = sheetCharacteristicScore cs (CharacteristicKey "str")

implicitAbility :: WeaponDB -> CharacterSheet -> CombatOption -> CombatLine
implicitAbility db cs co 
  | isNothing weapon = noLine
  | otherwise = defaultCL
        { combatLabel = combatName co
        , combatInit  = weaponInit w + ( fromMaybe 0 + weaponInit sh)
        , combatAtk   = atk w + ( fromMaybe 0 + atk sh)  + abscore
        , combatDef   = def w + ( fromMaybe 0 + def sh)
        , combatDam   = dam w + ( fromMaybe 0 + dam sh)  + abscore
        , combatRange = range w
        , combatLoad  = load w + ( fromMaybe 0 + load sh)
        } 
   where (abscore,abspec) = sheetAbilityScore cs $ AbilityKey $ fromJust ab
         weapon' = sheetPossession cs $ PossessionKey $ combatWeapon co
	 weapon = join $ fmap (weaponStat db ab) weapon'
         shield = sheetPossession cs $ PossessionKey $ combatShield co
	 sh = fmap (weaponStat db ab) shield
	 w = fromJust weapon



explicitAbility :: WeaponDB -> CharacterSheet -> CombatOption -> String -> CombatLine
explicitAbility db cs co ab
  | isNothing weapon = noLine
  | otherwise = defaultCL
        { combatLabel = combatName co
        , combatInit  = weaponInit w + ( fromMaybe 0 + weaponInit sh)
        , combatAtk   = atk w + ( fromMaybe 0 + atk sh)  + abscore
        , combatDef   = def w + ( fromMaybe 0 + def sh)
        , combatDam   = dam w + ( fromMaybe 0 + dam sh)  + abscore
        , combatRange = range w
        , combatLoad  = load w + ( fromMaybe 0 + load sh)
        } 
   where (abscore,abspec) = sheetAbilityScore cs $ AbilityKey $ fromJust ab
         weapon' = sheetPossession cs $ PossessionKey $ combatWeapon co
	 weapon = join $ fmap (weaponStat db ab) weapon'
         shield = sheetPossession cs $ PossessionKey $ combatShield co
	 sh = fmap (weaponStat db ab) shield
	 w = fromJust weapon
	 w = find ( (ab==) . weaponAbility ) ws

-- | Collapse a neste Maybe Maybe object to a single Maybe
join :: Maybe (Maybe a) -> Maybe a
join Nothing = Nothing
join (Just Nothing) = Nothing
join (Just (Just x)) = Just x

-- | Compute the table of combat stas from a list of `CombatOption` objects
computeCombatLines :: WeaponDB -> CharacterSheet -> [CombatOption] -> [CombatLine]
computeCombatLines db sh = map (computeCombatLine db sh)

-- | Compute the table of combat stas from a list of `CharacterSheet` objects
computeCombatStats :: WeaponDB -> CharacterSheet -> [CombatLine]
computeCombatStats db sh = computeCombatLines db sh $ combatList sh
