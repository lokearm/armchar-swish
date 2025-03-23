{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.DB.Weapon
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Types for Weapon and Armour tables
--
--
-----------------------------------------------------------------------------
module ArM.DB.Weapon where

import ArM.DB.CSV
import GHC.Generics
import Text.Read
import Data.Aeson
import qualified Data.Map as M

type WeaponDB = M.Map String Weapon
type ArmourDB = M.Map String Armour

-- | Create a `Data.Map.Map` of Weapon objects.  
-- The input is the output from `Data.CSV.csvFile`
weaponDB :: [[String]] -> WeaponDB
weaponDB = M.fromList . map ( \ x -> (weaponName x,x) ) . map fromCSVline

armourDB :: [[String]] -> ArmourDB
armourDB = M.fromList . map ( \ x -> (armourName x,x) ) . map fromCSVline

data Weapon = Weapon
    { weaponName :: String
    , weaponAbility :: String
    , weaponInit :: Int
    , atk :: Maybe Int
    , def :: Maybe Int
    , dam :: Maybe Int
    , str :: Maybe Int
    , range :: Maybe Int
    , load ::  Int
    , weaponCost :: String
    } deriving ( Show, Ord, Eq, Generic )
data Armour = Armour
    { armourName :: String
    , armourLoad :: Int
    , armourProtection :: Int
    , armourCost :: String
    } deriving ( Show, Ord, Eq, Generic )

instance FromJSON Weapon 
instance ToJSON Weapon 
instance FromJSON Armour 
instance ToJSON Armour 

instance ArMCSV Weapon where
   fromCSVline (x1:x2:x3:x4:x5:x6:x7:x8:_) =
      defaultObject { weaponName = x1 
                , weaponAbility = x2
                , weaponInit = read x3
                , atk = readMaybe x4
                , def = readMaybe x5
                , dam = readMaybe x5
                , str = readMaybe x6
                , range = readMaybe x7
                , load = read x7
                , weaponCost = x8
                }
   fromCSVline _ = defaultObject
   defaultObject = Weapon
                { weaponName = "" 
                , weaponAbility = ""
                , weaponInit = 0
                , atk = Nothing
                , def = Nothing
                , dam = Nothing
                , str = Nothing
                , range = Nothing
                , load = 0
                , weaponCost = "N/A"
                }
   getID = weaponName

instance ArMCSV Armour where
   fromCSVline (x1:x2:x3:x4:_) =
      defaultObject { armourName = x1 
                , armourLoad = read x2
                , armourProtection = read x3
                , armourCost = x4
                }
   fromCSVline _ = defaultObject
   defaultObject = Armour
                { armourName = "" 
                , armourLoad = 0
                , armourProtection = 0
                , armourCost = "N/A"
                }
   getID = armourName

