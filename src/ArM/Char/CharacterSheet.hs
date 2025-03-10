{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.CharacterSheet
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Char.CharacterSheet ( CharacterSheet(..)
                               , characterSheet
                               , filterCS
                               ) where

import ArM.Char.Trait
import ArM.Char.Internal.Character
-- import ArM.Char.Internal.Advancement
import GHC.Generics
import Data.Aeson
import Data.Maybe

-- import ArM.Debug.Trace

-- | The CharacterSheet object holds a Character State with separate fields
-- for different kinds of traits
data CharacterSheet = CharacterSheet 
         { csType :: CharacterType
         , vfList :: [ VF ]
         , charList :: [ Characteristic ]
         , abilityList :: [ Ability ]
         , artList :: [ Art ]
         , spellList :: [ Spell ]
         , reputationList :: [ Reputation ]
         , ptList :: [ PTrait ]
         , confList :: [ Confidence ]
         , csTraits :: [ Trait ]
         }  deriving (Eq,Show,Generic)

-- | A default CharacterSheet for internal use.
defaultSheet :: CharacterSheet 
defaultSheet = CharacterSheet 
         { csType = Magus
         , vfList = [ ]
         , charList = [ ]
         , abilityList = [ ]
         , artList = [ ]
         , spellList = [ ]
         , reputationList = []
         , ptList = []
         , confList = []
         , csTraits = [ ]
         }  

-- | Get the CharacterSheet from a given Character.
characterSheet :: Character -> CharacterSheet
characterSheet c | isNothing st = defaultSheet { csType = charType (concept c) }
                 | otherwise = cs  { csType = charType (concept c) }
    where st = state c
          cs = filterCS $ fromJust st 

instance ToJSON CharacterSheet where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CharacterSheet 

-- | Get the CharacterSheet corresponding to a given CharacterState.
filterCS :: CharacterState -> CharacterSheet
filterCS cs = defaultSheet  
                 { vfList = x1
                 , abilityList = x2
                 , artList = x3
                 , spellList = x4
                 , reputationList = x5
                 , ptList = x6
                 , charList = x7
                 , confList = x8
                 , csTraits = y8
                }
           where (x1,y1) = filterTrait $ traits cs
                 (x2,y2) = filterTrait y1
                 (x3,y3) = filterTrait y2
                 (x4,y4) = filterTrait y3
                 (x5,y5) = filterTrait y4
                 (x6,y6) = filterTrait y5
                 (x7,y7) = filterTrait y6
                 (x8,y8) = filterTrait y7
