{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.CharacterSheet
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  CharacterSheet is an alternative representation of CharacterState
--
-- CharacterSheet is a façade exposing lists for each kind of trait.
--
-----------------------------------------------------------------------------
module ArM.Char.CharacterSheet ( CharacterSheet(..)
                               , characterSheet
                               , filterCS
                               , getArtScore
                               , getAbilityScore
                               , getCharacteristicScore
                               , findTraitCS
                               , castingScore
                               , addCastingScores
                               , labTotals
                               , techniques
                               , forms
                               ) where

import ArM.Char.Trait
import ArM.Char.Spell
import ArM.Char.Internal.Character
import GHC.Generics
import Data.Aeson
import Data.Maybe
import Data.List

-- import ArM.Debug.Trace

-- | The CharacterSheet object holds a Character State with separate fields
-- for different kinds of traits
data CharacterSheet = CharacterSheet 
         { csType :: CharacterType
         , csAge :: Int
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
         , csAge = 0
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
                 | otherwise = cs { csType = charType (concept c) }
    where st = state c
          cs = filterCS st'
          st' = fromJust st 

instance ToJSON CharacterSheet where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CharacterSheet 

-- | Get the CharacterSheet corresponding to a given CharacterState.
filterCS :: CharacterState -> CharacterSheet
filterCS cs = defaultSheet  
                 { csType = charSType cs
                 , vfList = x1
                 , abilityList = x2
                 , artList = x3
                 , spellList = sortTraits x4
                 , reputationList = x5
                 , ptList = x6
                 , charList = x7
                 , confList = x8
                 , csTraits = y8
                 , csAge = age cs
                }
           where (x1,y1) = filterTrait $ traits cs
                 (x2,y2) = filterTrait y1
                 (x3,y3) = filterTrait y2
                 (x4,y4) = filterTrait y3
                 (x5,y5) = filterTrait y4
                 (x6,y6) = filterTrait y5
                 (x7,y7) = filterTrait y6
                 (x8,y8) = filterTrait y7

-- | Find a trait, given by a key, from a list of Trait objects.
findTrait :: (TraitLike a) => TraitKey -> [a] -> Maybe a
findTrait k = find ( (k==) . traitKey )


-- | Find a trait, given by a key, from the CharacterSheet.
-- This may not be in use. 
findTraitCS ::  TraitKey -> CharacterSheet -> Maybe Trait
findTraitCS (AbilityKey x) = (fmap toTrait) . findTrait (AbilityKey x) . abilityList
findTraitCS (ArtKey x) = (fmap toTrait) . findTrait (ArtKey x) . artList
findTraitCS (SpellKey x y z) = (fmap toTrait) . findTrait (SpellKey x y z) . spellList
findTraitCS (CharacteristicKey x) = (fmap toTrait) . findTrait (CharacteristicKey x) . charList
findTraitCS _ = \ _ -> Nothing

-- | Get the score and speciality in a given ability.
getAbilityScore :: CharacterSheet -> TraitKey -> (Int,Maybe String)
getAbilityScore cs k | isNothing x = (0,Nothing)
                     | otherwise = (abilityScore x', speciality x') 
     where x = ( findTrait k . abilityList ) cs
           x' = fromJust x
-- | Get a given art score 
getArtScore :: CharacterSheet -> TraitKey -> Int
getArtScore cs k | isNothing x = 0
                 | otherwise = artScore x'
     where x = ( findTrait k . artList ) cs
           x' = fromJust x
-- | Get a given characteristic score 
getCharacteristicScore :: CharacterSheet -> TraitKey -> Int
getCharacteristicScore cs k | isNothing x = 0
                 | otherwise = charScore x'
     where x = ( findTrait k . charList ) cs
           x' = fromJust x


-- | Helper for `castingScore`
castingScore' :: CharacterSheet -> [TraitKey] -> [TraitKey] -> Int
castingScore' cs ts fs = t + f + sta
    where t = minl $  map (getArtScore cs) ts
          f = minl $  map (getArtScore cs) fs
          sta = getCharacteristicScore cs (CharacteristicKey "Sta")
          minl [] = 0
          minl (x:xs) = foldl min x xs

-- | Return the Casting Score for a given spell.
-- The function depends both on the Spell trait from the CharacterSheet
-- and a generic spell description from a SpellDB.
castingScore :: SpellDB    -- ^ Spell DB with general descriptions of the spells
             -> CharacterSheet -- ^ Current character sheet
             -> TraitKey       -- ^ Key identifying the spell
             -> Int            -- ^ Computed casting score
castingScore db cs k | isNothing rec' =   0
                     | isNothing sp' =   0
                     | otherwise =  castingScore' cs ts fs + mf ( getTrait sp)
   where sp' = findTraitCS k cs
         sp = fromJust sp'
         mf Nothing = 0
         mf (Just x) = masteryScore x
         rec' = spellLookup k db
         rec = fromJust rec'
         ts = (ArtKey $ technique rec):(map ArtKey $ techniqueReq rec)
         fs = (ArtKey $ form rec):(map ArtKey $ formReq rec)

addCastingScores :: SpellDB -> CharacterSheet -> CharacterSheet
addCastingScores db cs =  cs { spellList = spellList' }
   where spellList' = map (addCastingScore db cs) (spellList cs)
addCastingScore :: SpellDB -> CharacterSheet -> Spell -> Spell
addCastingScore db cs sp =  sp { spellCastingScore = sc }
   where sc = Just $ castingScore db cs (traitKey sp) 


-- | Return the Lab Total a given TeFo combo.
labTotal :: CharacterSheet -- ^ Current character sheet
             -> TraitKey       -- ^ Key identifying the technique
             -> TraitKey       -- ^ Key identifying the form
             -> Int            -- ^ Computed lab total
labTotal cs te fo = ts + fs + int + mt
   where ts = getArtScore cs te
         fs = getArtScore cs fo
         int = getCharacteristicScore cs (CharacteristicKey "Int" ) 
         (mt,_) = getAbilityScore cs (AbilityKey "Magic Theory" ) 

labTotals :: CharacterSheet -- ^ Current character sheet
             -> [[Int]]     -- ^ Computed lab totals 
labTotals cs = [ [ labTotal cs te fo | te <- techniques ] | fo <- forms ]

techniques :: [ TraitKey ]
techniques = [ ArtKey te | te <- [ "Cr", "In", "Mu", "Pe", "Re" ] ]
forms :: [ TraitKey ]
forms = [ ArtKey fo | fo <- [ "An", "Aq", "Au", "Co", "He", "Ig", "Im", "Me", "Te", "Vi" ] ]
