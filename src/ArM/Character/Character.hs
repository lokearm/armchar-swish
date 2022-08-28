{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- The CharacterSheet data type and corresponding functions.
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
module ArM.Character.Character ( CharacterSheet(..)
                               , characterFromGraph
                               , ToRDFGraph(..)
                               , FromRDFGraph(..)
                               , advanceCharacter
                               , getInitialCS
                               , makeCGraph
                               ) where

import ArM.Rules.Aux
import ArM.Internal.Aux

import           Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import qualified Swish.RDF.VarBinding as VB 
import           Swish.VarBinding  (vbMap)
import           Data.Maybe (fromJust)
import           Data.List (sort)
import ArM.Rules (makeGraph)
import ArM.Resources
import ArM.Character.Trait
import ArM.Character.Advancement
import ArM.KeyPair
import ArM.Types.Character
import ArM.Types.Season

import qualified ArM.Rules.Record as R

-- import Debug.Trace
trace x y = y


makeCGraph schema = R.prepareRecord schema . makeRDFGraph


-- | Get initial CharacterSheet, before *any* advancements.
getInitialCharacter ::
    Character          -- ^ Character Object
    -> CharacterSheet  -- ^ Empty charactersheet (age 0) for the character
getInitialCharacter c = defaultCS {
            csID = characterID c,
            born = getIntProperty (armRes "hasBirthYear") $ characterData c,
            csMetadata = characterData  c
         }

-- |
-- = Advancement

-- | Given a character sheet and a sorted list of advancements,
-- apply all the advancements in order and produce a list
-- of character sheets for every step
advanceList :: CharacterSheet -> [Advancement] -> [CharacterSheet]
advanceList _ [] = []
advanceList cs (x:xs) = cs' : advanceList cs' xs
            where cs' = advanceCharacter cs x 

-- | apply a given Advancement to a given CharacterSheet
advanceCharacter :: CharacterSheet -> Advancement -> CharacterSheet 
advanceCharacter cs adv = trace ("advanceCharacter\n"++(show cs)++(show $ rdfid adv)) $
     cs { sheetID = Nothing
        , csTime = nextCharTime $ advTime adv
        , csTraits = advanceTraitList (csTraits cs) (sort $ traits adv)
        , csItems = advanceTraitList (csItems cs) (items adv)
     }



-- |
-- = Get Character ID from a graph

-- | Find all characters in a given graph.  Auxiliary for `characterFromGraph`.
characterFromGraph' :: RDFGraph -> [VB.RDFVarBinding]
characterFromGraph' = Q.rdfQueryFind
             $ listToRDFGraph  [ arc cVar typeRes armCharacter ]
-- | Get the labels of all characters in a given graph.
characterFromGraph :: RDFGraph -> [RDFLabel]
characterFromGraph = uniqueSort . f . map (`vbMap` cVar) . characterFromGraph' 
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs

-- |
-- = Get Character Metadata

-- | Construct a query to get all
-- arm:CharacterProperty triples for a given subject.
query c = listToRDFGraph 
   [ arc c (G.Var "property") (G.Var "value")
   , arc (G.Var "property") typeRes armCharacterProperty
   , arc (G.Var "property") labelRes  (G.Var "label") ]

-- | Make a list of metadata, where each data item is
-- a triple consisting of URI, Label, and Value.
-- The inputs are an 'RDFGraph' g and a string naming an RDF resource,
-- either as a prefixed name or as a full URI in angled brackets (<uri>).
getCharacterMetadata :: G.RDFGraph -> RDFLabel -> KeyPairList
getCharacterMetadata g s = KeyPairList $ map keypairFromBinding
                          $  Q.rdfQueryFind (query s) g

getInitialCS :: RDFGraph -> CharacterSheet
getInitialCS = getInitialCharacter . getCharacter
getCharacter :: RDFGraph -> Character
getCharacter g = fromRDFGraph g label 
   where label = head $ characterFromGraph g

-- |
-- = Instances - Load Character object from graph

instance FromRDFGraph Character where
   fromRDFGraph g label = defaultCharacter {
                 characterID = label,
                 characterData = getCharacterMetadata g label
                 }
