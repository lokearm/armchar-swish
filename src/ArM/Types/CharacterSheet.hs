{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.CharacterSheet
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- The Character and CharacterSheet data types and corresponding functions.
--
-- This module provides the functions to get character sheets
-- from character data.
--
-- Pre-game character design has not yet been implemented.
--
-- The main function provided is the `getAllCS` function
-- which is used to generate the data structure in `ArM.STM`.
-- This relies on auxiliary functions to advance the character,
-- and further on similar functions in `ArM.Character.Trait` to
-- advance traits.
--
-- Metadata can be extracted for either a Character or a CharacterSheet
-- by giving just the ID.  Metadata can also be extracted as part of
-- a Character object using `fromRDFGraph`.
-- There is also a function, `characterFromGraph` to get the IDs of
-- all Character objects in a graph.
--
-----------------------------------------------------------------------------
module ArM.Types.CharacterSheet ( CharacterSheet(..)
                           , advanceCharacter
                           , getInitialCS
                           , extractBaseCharacterGraph 
                           ) where

import Swish.RDF.Graph as G
import           Data.List (sort)

import ArM.KeyPair
import ArM.Types.Character
import ArM.Resources
import ArM.BlankNode
import ArM.Rules.Aux
import ArM.Types.Advancement
import ArM.Types.Season
import ArM.Types.RDF
import ArM.Types.Trait
import Data.Aeson
import Data.Aeson.Key

import ArM.Debug.NoTrace

-- | 
-- = Character Sheet

data CharacterSheet = CharacterSheet {
      csID :: RDFLabel,
      -- ^ Character ID (i.e. same ID for every season)
      sheetID :: Maybe RDFLabel,  
      -- ^ ID of the Character Sheet, usually Nothing suggesting a blank node
      csTime :: CharTime,  -- ^ Current Year
      born     :: Int,      -- ^ Year of Birth
      csTraits :: [Trait],  -- ^ List of traits (abilities, spells, etc.)
      csMetadata :: KeyPairList
      -- ^ Metadata, i.e. data which are not traits or items.
      }  deriving (Eq)
instance Show CharacterSheet where
    show cs = "**" ++ show (csID cs) ++ "**\n" 
           ++ "-- " ++ ( showSheetID ) cs ++ "\n"
           ++ "Traits:\n" ++ showw ( csTraits cs )
           ++ "Metadata Triples:\n" ++ show ( csMetadata cs )
        where showw [] = ""
              showw (x:xs) = "  " ++ show x ++ "\n" ++ showw xs
defaultCS :: CharacterSheet
defaultCS = CharacterSheet {
         csID = noSuchCharacter,
         sheetID = Nothing,
         csTime = defaultCharTime,
         born = 0,
         csTraits = [],
         csMetadata = KeyPairList []
       }  

showSheetID :: CharacterSheet -> String
showSheetID = f . sheetID
    where f Nothing = "no sheet ID"
          f (Just x) = show x


instance ToRDFGraph CharacterSheet where
   makeRDFGraph cs = trace msg $
         ( listToRDFGraph  . fst . runBlank ( csToArcListM cs' ) )
         ("charsheet",1)
      where cs' = cs { csMetadata = KeyPairList $ a xs }
            KeyPairList xs = csMetadata cs
            aAge x ys  
                   | x == 0 = ys
                   | otherwise = KeyValuePair (armRes "hasAge") (litInt x):ys
            aYear Nothing ys   = ys
            aYear (Just x) ys  = KeyValuePair (armRes "inYear") (litInt x):ys
            aSeason x ys  
                   | x == "" = ys
                   | otherwise = KeyValuePair (armRes "atSeason") (litString x):ys
            age' Nothing _ = 0
            age' (Just y) b | b == 0 = 0 
                            | otherwise = y - b
            age = age' (hasYear cs) (born cs)
            a = aAge age . aYear (hasYear cs) . aSeason (hasSeason cs)
            msg = "makeRDFGraph for CharacterSheet (" ++ show traitcount ++ " traits)"
            traitcount = length $ csTraits cs

csToArcListM :: CharacterSheet -> BlankState [RDFTriple]
csToArcListM cs = do
          x <- getSheetIDM $ sheetID cs
          tsm <- fixBlanksM $ csTraits cs
          let ht = map ( \ y -> arc x htRes (traitID y) ) tsm
          let ts =  map traitContents tsm
          let metadata = keyvalueToArcList x (fromKeyPairList $ csMetadata cs)
          let ms = metadata ++ ht 
          let ct = arc x (armRes "ïsCharacter") (csID cs)
          let ct1 = arc x typeRes (armRes "CharacterSheet")
          return $ ct1:ct:foldr (++) ms ts

getSheetIDM :: Maybe RDFLabel -> BlankState RDFLabel
getSheetIDM Nothing = getBlank
getSheetIDM (Just x) = return x

instance ToJSON CharacterSheet where 
    toJSON cs = object (c:x:xs)
       where x = (fromString "arm:hasTrait") .= (toJSON (csTraits cs))
             xs = map tripleToJSON (fromKeyPairList $ csMetadata cs)
             c = (fromString "arm:isCharacter") .= (show $ csID cs)


instance HasTime CharacterSheet where
    timeOf = csTime

-- |
-- = Advancement 


-- | apply a given Advancement to a given CharacterSheet
advanceCharacter :: CharacterSheet -> Advancement -> CharacterSheet 
advanceCharacter cs adv = 
     cs { sheetID = Nothing
        , csTime = nextCharTime $ advTime adv
        , csTraits = advanceTraitList (csTraits cs) (sort $ traits adv)
     }

-- | Create an initial character sheet (age 0) without any traits added.
-- The function assumes that the give Graph contains definitions for a
-- single character resource.
getInitialCS :: RDFGraph -- ^ RDFGraph containing the character
      -> CharacterSheet  -- ^ Empty charactersheet (age 0) for the character
getInitialCS = getInitialCharacter . getCharacter
   where getInitialCharacter c = defaultCS {
            csID = characterID c,
            born = getIntProperty (armRes "hasBirthYear") $ characterData c,
            csMetadata = characterData  c
         }
