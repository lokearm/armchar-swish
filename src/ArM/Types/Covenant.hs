{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Characters and functions for advancement.
--
-- This module contains types to process characters, including 
-- persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.Types.Covenant where

import GHC.Generics
import Data.Aeson
import Data.Maybe

import ArM.Char.Trait
import ArM.Types.Advancement
import ArM.Types.KeyPair
import ArM.Helper

-- import ArM.Debug.Trace


data CovenantConcept = CovenantConcept 
         { covName :: String
         , covConcept :: Maybe String
         , covFounded :: Maybe Int
         , covData :: KeyPairList
       }  deriving (Eq,Generic)

-- | Default (empty) character concept object.
defaultCovConcept :: CovenantConcept 
defaultCovConcept = CovenantConcept { covName = "Player Covenant"
                                  , covConcept = Nothing
                                  , covFounded = Nothing
                                  , covData = KeyPairList []
       }  

instance ToJSON CovenantConcept where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CovenantConcept 

instance Show CovenantConcept where
   show c = covName c ++ " covenant (est. " ++ sf (covFounded c) ++ ") "
         ++ (fromMaybe "" $ covConcept c) ++ "\n"
         ++ ( show $ covData c )
    where sf Nothing = "-"
          sf (Just x ) = show x

