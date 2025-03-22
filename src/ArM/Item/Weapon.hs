-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Item.Weapon
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Types for Weapons
--
--
-----------------------------------------------------------------------------
module ArM.Char.Trait where

import ArM.Helper

import Data.Maybe 

import ArM.Debug.Trace


data Weapon = Weapon
    { weaponName :: String
    , weaponAbility :: String
    , init :: Int
    , atk :: Maybe Int
    , def :: Maybe Int
    , dam :: Maybe Int
    , str :: Maybe Int
    , range :: Maybe Int
    , load ::  Int
    , weaponCost :: String
    } deriving ( Show, Ord, Eq )
data Armour = Armour
    { armourName :: String
    , armourLoad :: Int
    , armourProtection :: Int
    , armourCost :: String
    } deriving ( Show, Ord, Eq )

data Possesion = WeaponPossession Weapon Int
               | Possession String Int
	       | VisPossession String Int
	       | ArmourPossession Armour Int
