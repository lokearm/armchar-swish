{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Character
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
module ArM.Char.Character ( Character(..)
                          , defaultCharacter
                          , CharacterConcept(..)
                          , defaultConcept
                          , CharacterState(..)
                          , KeyPairList(..)
                          , KeyPair(..)
                          , FieldValue(..)
                          , prepareCharacter
                          , Advancement(..)
                          , fullName
                          , fullConceptName
                          ) where

import GHC.Generics
import Data.Aeson
-- import Data.Maybe (fromJust,isNothing,isJust)
-- import Data.Aeson.Types (Parser)

import ArM.Char.Trait
import ArM.Char.Advancement
import ArM.Char.Internal.KeyPair
import ArM.Debug.Trace
-- import ArM.Types.Season
import ArM.Helper




-- |
-- = CharacterConcept

data CharacterConcept = CharacterConcept 
         { name :: String
         , house :: Maybe String
         , charGlance :: KeyPairList
         , charData :: KeyPairList
       }  deriving (Eq,Generic)

-- | Default (empty) character concept object.
defaultConcept :: CharacterConcept 
defaultConcept = CharacterConcept { name = "John Doe"
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
         { charTime :: CharTime
         , traits :: [ Trait ]
         }  deriving (Eq,Generic)

-- | Default (empty) character state object.
defaultCS :: CharacterState 
defaultCS = CharacterState 
         { charTime = Nothing
         , traits = [ ]
         }  

instance ToJSON CharacterState where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CharacterState where
    parseJSON = withObject "CharacterState" $ \v -> CharacterState
        <$> v .:? "charTime"
        <*> fmap maybeList ( v .:? "traits" )

-- |
-- = Character

data Character = Character 
         { charID :: String
         , concept :: CharacterConcept
         , state :: Maybe CharacterState
         , pregameDesign :: [ Advancement ]
         , pregameAdvancement :: [ AugmentedAdvancement ]
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

-- |
-- = Advancement


-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: CharacterState -> Advancement -> AugmentedAdvancement
prepareAdvancement _ = prepareAdvancementVF 


-- | Apply advancement
applyAdvancement :: Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyAdvancement a cs = (a',cs')
    where a' = prepareAdvancement cs a
          cs' = cs { charTime = season a, traits = new }
          new =  advance change tmp
          tmp =  advance inferred old 
          change = sortTraits $ changes a'
          inferred = trace (">" ++ show a') $ inferredTraits a'
          old = traits cs

-- | Apply a list of advancements
applyAdvancements :: [Advancement] -> CharacterState -> ([(AugmentedAdvancement,Advancement)],CharacterState)
applyAdvancements a cs = applyAdvancements' ([],a,cs)
applyAdvancements' :: ([(Advancement,Advancement)],[Advancement],CharacterState)
                   -> ([(Advancement,Advancement)],CharacterState)
applyAdvancements' (xs,[],cs) = (xs,cs)
applyAdvancements' (xs,y:ys,cs) = applyAdvancements' ((a',y):xs,ys,cs')
    where (a',cs') = applyAdvancement y cs


-- | Compute the initial state if no state is recorded.
prepareCharacter :: Character -> Character
prepareCharacter c 
            | state c /= Nothing = c
            | otherwise = c { state = Just cs
                            , pregameDesign = fst $ unzip xs
                            }
            where as = pregameAdvancement  c 
                  (xs,cs) = applyAdvancements as defaultCS

