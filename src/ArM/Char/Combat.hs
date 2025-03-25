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
import ArM.DB.Weapon

import GHC.Generics
-- import Data.Maybe

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
    } 
data SoakLine = SoakLine 
    { soakLabel    :: String
    , soakScore    :: Int
    , soakLoad     ::  Int
    } deriving (Show, Eq, Generic)


computeCombatLine :: WeaponDB -> CharacterSheet -> CombatOption -> CombatLine
computeCombatLine _ _ _ = defaultCL

computeCombatLines :: WeaponDB -> CharacterSheet -> [CombatOption] -> [CombatLine]
computeCombatLines db sh = map (computeCombatLine db sh)

computeCombatStats :: WeaponDB -> CharacterSheet -> [CombatLine]
computeCombatStats db sh = computeCombatLines db sh $ combatList sh
