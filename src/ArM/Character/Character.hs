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
module ArM.Character.Character ( characterFromGraph
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
import           Data.List (sort)
import ArM.Resources
import ArM.Character.Trait
import ArM.Types.Advancement
import ArM.KeyPair
import ArM.Types.RDF
import ArM.Types.Character
import ArM.Types.Season

import qualified ArM.Rules.Record as R

import ArM.NoTrace


makeCGraph :: RDFGraph -> CharacterSheet -> RDFGraph
makeCGraph schema = R.prepareRecord schema . makeRDFGraph

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

-- | Find all characters in a given graph.
-- Auxiliary for `characterFromGraph`.
characterFromGraph' :: RDFGraph -> [VB.RDFVarBinding]
characterFromGraph' = Q.rdfQueryFind
             $ listToRDFGraph  [ arc cVar typeRes armCharacter ]

-- | Get the labels of all characters in a given graph.
characterFromGraph :: RDFGraph -> [RDFLabel]
characterFromGraph = uniqueSort . f . map (`vbMap` cVar) 
                  . characterFromGraph' 
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs

-- |
-- = Get Character Metadata

getInitialCS :: RDFGraph -- ^ RDFGraph containing the character
    -> CharacterSheet  -- ^ Empty charactersheet (age 0) for the character
getInitialCS = getInitialCharacter . getCharacter
   where getInitialCharacter c = defaultCS {
            csID = characterID c,
            born = getIntProperty (armRes "hasBirthYear") $ characterData c,
            csMetadata = characterData  c
         }

getCharacter :: RDFGraph -> Character
getCharacter g = fromRDFGraph g label 
   where label = head $ characterFromGraph g

