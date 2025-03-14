{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Internal.Character
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
module ArM.Char.Internal.Character ( Character(..)
                          , defaultCharacter
                          , CharacterType(..)
                          , CharacterConcept(..)
                          , defaultConcept
                          , CharacterState(..)
                          , KeyPairList(..)
                          , KeyPair(..)
                          , FieldValue(..)
                          , fullName
                          , fullConceptName
                          , isGrog
                          , isMagus
                          , defaultCS
                          ) where

import GHC.Generics
import Data.Aeson
-- import Data.Aeson.Types (Parser)

import ArM.Char.Trait
import ArM.Char.Internal.Advancement
import ArM.Char.Internal.KeyPair
-- import ArM.Debug.Trace
-- import ArM.Types.Season
import ArM.Helper



isGrog :: Character -> Bool
isGrog c | charType (concept c) == Grog = True
         | otherwise = False
isMagus :: Character -> Bool
isMagus c | charType (concept c) == Magus = True
          | otherwise = False

-- |
-- = CharacterConcept

data CharacterType = Magus | Companion | Grog
       deriving (Eq,Generic,Show)
instance ToJSON CharacterType
instance FromJSON CharacterType

data CharacterConcept = CharacterConcept 
         { name :: String
         , charType :: CharacterType
         , briefConcept :: Maybe String
         , quirk :: Maybe String
         , appearance :: Maybe String
         , born :: Maybe Int
         , player :: Maybe String
         , house :: Maybe String
         , charGlance :: KeyPairList
         , charData :: KeyPairList
       }  deriving (Eq,Generic)

-- | Default (empty) character concept object.
defaultConcept :: CharacterConcept 
defaultConcept = CharacterConcept { name = "John Doe"
                                  , charType = Magus
                                  , briefConcept = Nothing
                                  , quirk = Nothing
                                  , appearance = Nothing
                                  , born = Nothing
                                  , player = Nothing
                                  , house = Nothing
                                  , charGlance = KeyPairList []
                                  , charData = KeyPairList []
       }  

instance ToJSON CharacterConcept where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CharacterConcept where
    parseJSON = withObject "CharacterConcept" $ \v -> CharacterConcept
        <$> v .: "name"
        <*> v .: "charType"
        <*> v .:? "concept"
        <*> v .:? "quirk"
        <*> v .:? "appearance"
        <*> v .:? "born"
        <*> v .:? "player"
        <*> v .:? "house"
        <*> v .: "charGlance"
        <*> v .: "charData"

instance Show CharacterConcept where
   show c = fullConceptName c ++ "\n"
         ++ ( show $ charGlance c ) ++ ( show $ charData c )

-- | Return the name of the character as a string, including house affiliation
-- if defined.
fullConceptName :: CharacterConcept -> String
fullConceptName c = name c ++ (f $ house c)
      where f Nothing = ""
            f (Just x) | take 2 x == "ex" = " " ++ x
                       | otherwise  = " ex " ++ x


-- |
-- = CharacterState

data CharacterState = CharacterState 
         { charTime :: SeasonTime
         , charSType :: CharacterType
         , age :: Int
         , traits :: [ Trait ]
         }  deriving (Eq,Generic,Show)

-- | Default (empty) character state object.
defaultCS :: CharacterState 
defaultCS = CharacterState 
         { charTime = NoTime
         , charSType = Magus
         , age = 0
         , traits = [ ]
         }  

instance ToJSON CharacterState where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CharacterState where
    parseJSON = withObject "CharacterState" $ \v -> CharacterState
        -- <$> v .:? "charTime"
        <$> fmap parseSeasonTime ( v .:? "charTime" )
        <*> v .: "charType" 
        <*> v .: "age" 
        <*> fmap maybeList ( v .:? "traits" )

-- |
-- = Character

data Character = Character 
         { charID :: String
         , concept :: CharacterConcept
         , state :: Maybe CharacterState
         , pregameDesign :: [ AugmentedAdvancement ]
         , pregameAdvancement :: [ Advancement ]
         , pastAdvancement :: [ AugmentedAdvancement ]
         , futureAdvancement :: [ Advancement ]
         }  deriving (Eq,Generic)


-- | Default (empty) character object.
defaultCharacter :: Character 
defaultCharacter = Character { charID = "N/A"
                             , concept = defaultConcept
                             , state = Nothing
                             , pregameDesign = [ ]
                             , pregameAdvancement = [ ]
                             , pastAdvancement = [ ]
                             , futureAdvancement = [ ]
       }  

instance Show Character where
   show = show . concept 

-- | Return the name of the character as a string, including house affiliation
-- if defined.
fullName :: Character -> String
fullName = fullConceptName . concept

instance ToJSON Character where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Character where
    parseJSON = withObject "Character" $ \v -> Character
        <$> v .: "charID"
        <*> v .: "concept"
        <*> v .:? "state" 
        <*> fmap maybeList ( v .:? "pregameDesign" )
        <*> fmap maybeList ( v .:? "pregameAdvancement" )
        <*> fmap maybeList ( v .:? "pastAdvancement" )
        <*> fmap maybeList ( v .:? "futureAdvancement" )

