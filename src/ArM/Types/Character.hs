{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Character
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
module ArM.Types.Character ( Character(..)
                           , CharacterSheet(..)
                           , getCharacter
                           , advanceCharacter
                           , getInitialCS
                           , extractBaseCharacterGraph 
                           ) where

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import           Swish.VarBinding  (vbMap)
import           Data.List (sort)
import           Data.Maybe (fromJust)

import ArM.KeyPair
import ArM.Resources
import ArM.BlankNode
import ArM.Rules.Aux
import ArM.Internal.Aux
import ArM.Types.Advancement
import ArM.Types.Season
import ArM.Types.RDF
import ArM.Types.Trait
import Data.Aeson
import Data.Aeson.Key

import ArM.Debug.Trace

-- | 
-- = Character

data Character = Character {
         characterID :: RDFLabel,
         characterData :: KeyPairList
       }  deriving (Eq)
instance Show Character where
    show cs = "**" ++ show (characterID cs) ++ "**\n" 
           ++ "Metadata Triples:\n" ++ show ( characterData cs )
defaultCharacter :: Character 
defaultCharacter = Character {
         characterID = noSuchCharacter,
         characterData = KeyPairList []
       }  

instance ToRDFGraph Character where
   makeRDFGraph cs = listToRDFGraph  ( ct:ms )
       where x = characterID cs
             ms = keyvalueToArcList x (fromKeyPairList $ characterData cs)
             ct = arc x typeRes (armRes  "Character")

instance ToJSON Character where 
    toJSON c = toJSON $ p x xs
        where x = KeyValuePair (armRes "isCharacter") $ characterID c
              xs = characterData c 
              p y (KeyPairList ys) = KeyPairList (y:ys) 

instance FromJSON Character where 
    parseJSON val = fmap kpToChar $ parseJSON val
       where kpToChar (KeyPairList xs) = defaultCharacter {
                      characterID = fromJ $ getProperty (armRes "isCharacter") xs,
                      characterData = KeyPairList xs
                      }
             fromJ Nothing = noSuchCharacter
             fromJ (Just x) = x

instance FromRDFGraph Character where
   fromRDFGraph g label = defaultCharacter {
                 characterID = label,
                 characterData = getCharacterMetadata g label
                 }

-- | Make a list of metadata, where each data item is
-- a triple consisting of URI, Label, and Value.
-- The inputs are an 'RDFGraph' g and a string naming an RDF resource,
-- either as a prefixed name or as a full URI in angled brackets (<uri>).
getCharacterMetadata :: G.RDFGraph -> RDFLabel -> KeyPairList
getCharacterMetadata g s = KeyPairList $ map keypairFromBinding
                          $  Q.rdfQueryFind (query s) g

-- | Construct a query to get all
-- arm:CharacterProperty triples for a given subject.
query :: RDFLabel -> RDFGraph 
query c = listToRDFGraph 
   [ arc c (G.Var "property") (G.Var "value")
   , arc (G.Var "property") typeRes armCharacterProperty
   , arc (G.Var "property") labelRes  (G.Var "label") ]

extractBaseCharacterGraph :: RDFGraph -> RDFLabel -> RDFGraph 
extractBaseCharacterGraph g c = listToRDFGraph
                              $ map f $  Q.rdfQueryFind (query c) g
   where f vb = arc c (fromJust $ vbMap vb (G.Var "property"))
                      (fromJust $ vbMap vb (G.Var "value"))


-- | 
-- = Character Sheet

data CharacterSheet = CharacterSheet {
      csID :: RDFLabel,
      -- ^ Character ID (i.e. same ID for every season)
      sheetID :: Maybe RDFLabel,  
      -- ^ ID of the Character Sheet, usually Nothing suggesting a blank node
      csTime :: CharTime,  -- ^ Current Year
      born     :: Int,      -- ^ Year of Birth
      csItems :: [Trait],    -- ^ List of possessions (weapons, equipment)
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
         csItems = [],
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
          ism <- fixBlanksM $ csItems $ trace ("#tsm = " ++ show (length tsm)) cs
          let ht = map ( \ y -> arc x htRes (traitID y) ) tsm
          let hi = map ( \ y -> arc x (armRes "hasPossession") (traitID y) ) ism
          let ts =  map traitContents tsm
          let is =  map traitContents ism
          let metadata = keyvalueToArcList x (fromKeyPairList $ csMetadata cs)
          let ms = metadata ++ hi ++ ht 
          let ct = arc x (armRes "ïsCharacter") (csID cs)
          let ct1 = arc x typeRes csRes 
          let ms1 = foldr (++) ms ts
          return $ ct1:ct:foldr (++) ms1 is

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
        , csItems = advanceTraitList (csItems cs) (items adv)
     }

-- |
-- = Get Character from a graph

-- | Get the labels of all characters in a given graph.
characterFromGraph :: RDFGraph -> [RDFLabel]
characterFromGraph = uniqueSort . f . map (`vbMap` cVar) 
                . Q.rdfQueryFind ( listToRDFGraph  [ arc cVar typeRes armCharacter ] )
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs

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

-- | Get a character object from an RDFGraph object.
-- The function assumes that the give Graph contains definitions for a
-- single character resource.  If none is found, an empty (default) character
-- is returned.  If multiple characters are defined, the first one (arbitrarily)
-- is used.
-- This is only used as an auxiliary to `getInitialCS`.
getCharacter :: RDFGraph -> Character
getCharacter g = f $ lab $ characterFromGraph g
   where lab [] = trace "getCharacter finds no character" Nothing
         lab (x:_) = trace ( "getCharacter found a character: " ++ show x) $ Just x
         f Nothing = defaultCharacter
         f (Just x) = fromRDFGraph g $ x 

