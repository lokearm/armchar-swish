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
                           , getCharacter
                           ) where

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import           Swish.VarBinding  (vbMap)
-- import           Data.Maybe (fromJust)

import ArM.Swish.KeyPair
import ArM.Swish.Resources
import ArM.Rules.Aux
import ArM.Types.RDF
import ArM.Internal.Aux
import Data.Aeson

import ArM.Debug.NoTrace

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

{-
extractBaseCharacterGraph :: RDFGraph -> RDFLabel -> RDFGraph 
extractBaseCharacterGraph g c = listToRDFGraph
                              $ map f $  Q.rdfQueryFind (query c) g
   where f vb = arc c (fromJust $ vbMap vb (G.Var "property"))
                      (fromJust $ vbMap vb (G.Var "value"))
-}

-- |
-- = Get Character from a graph

-- | Get the labels of all characters in a given graph.
characterFromGraph :: RDFGraph -> [RDFLabel]
characterFromGraph = uniqueSort . f . map (`vbMap` cVar) 
                . Q.rdfQueryFind ( listToRDFGraph  [ arc cVar typeRes armCharacter ] )
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs




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
