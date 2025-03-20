{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Cov.Covenant
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
module ArM.Cov.Covenant where

import GHC.Generics
import Data.Aeson
import Data.Maybe

import ArM.Char.Trait
import ArM.Char.Character
-- import ArM.Types.Character
-- import ArM.Types.Advancement
-- import ArM.Types.KeyPair
-- import ArM.Helper

-- import ArM.Debug.Trace

-- | ID of a character.
-- This is currently implemented as the name.
data CharacterID = CharacterID String
    deriving ( Show, Ord, Eq, Generic )

instance ToJSON CharacterID
instance FromJSON CharacterID

-- | get the ID of a character.
characterID :: Character -> CharacterID
characterID = CharacterID . charID

data Covenant = Covenant 
         { covenantConcept :: CovenantConcept
         , covenantState :: CovenantState
         , pastCovAdvancement :: [ CovAdvancement ]
         , futureCovAdvancement :: [ CovAdvancement ]
       }  deriving (Eq,Generic,Show)
instance ToJSON Covenant 
instance FromJSON Covenant 

data CovenantConcept = CovenantConcept 
         { covName :: String
         , covConcept :: Maybe String
         , covFounded :: Maybe Int
         , covAppearance :: Maybe String
         , covData :: KeyPairList
       }  deriving (Eq,Generic)

-- | Default (empty) covenant concept object.
defaultCovConcept :: CovenantConcept 
defaultCovConcept = CovenantConcept { covName = "Player Covenant"
                                  , covConcept = Nothing
                                  , covFounded = Nothing
                                  , covAppearance = Nothing
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

data CovenantState = CovenantState 
         { covTime :: SeasonTime
         , covenFolkID :: [ CharacterID ]
         , covenFolk :: [ Character ]
         , library :: [ Book ]
       }  deriving (Eq,Generic,Show)


instance ToJSON CovenantState
instance FromJSON CovenantState

-- | Advancement (changes) to a covenant.
data CovAdvancement = CovAdvancement 
     { caSeason :: SeasonTime    -- ^ season or development stage
     , caNarrative :: Maybe String -- ^ freeform description of the activities
     , joining :: [ CharacterID ]
     , leaving :: [ CharacterID ]
     , acquired :: [ Book ]
     , lost :: [ Book ]
     }
   deriving (Eq,Generic,Show)

defaultAdv :: CovAdvancement 
defaultAdv = CovAdvancement 
     { caSeason = NoTime
     , caNarrative = Nothing
     , joining = []
     , leaving = []
     , acquired = []
     , lost = []
     }
instance ToJSON CovAdvancement
instance FromJSON CovAdvancement

data Book = Book
         { title :: String
         , topic :: TraitKey
         , quality :: Int
         , bookLevel :: Maybe Int
         , author :: String
         , year :: Int
         , annotation :: String
       }  deriving (Eq,Generic,Show)
instance ToJSON Book
instance FromJSON Book
