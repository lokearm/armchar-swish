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
                               , getGameStartCharacter
                               , getAllCS
                               , makeCharGen
                               , characterFromGraph
                               , ToRDFGraph(..)
                               , FromRDFGraph(..)
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
import ArM.Types.CharGen

import qualified ArM.Rules.Record as R

-- import Debug.Trace
trace x y = y

-- |
-- = Making Character Sheets
makeCharDev :: G.RDFGraph  -- ^ Schema graph
           -> G.RDFGraph  -- ^ Resource graph
           -> G.RDFGraph  -- ^ Raw character graph
           -> CharacterSheet  -- ^ Character Sheet at the start of development
           -> CharGen         -- ^ Resulting datastructure
makeCharDev schema res1 g0 cs0 = CharGen 
             { charID = clab
             , charName = ""
             , charGraph = g1
             , rawGraph = g0
             , baseSheet = cs0
             , charSheets = makeCS schema as "Game Start" cs0
             }
     where as = reverse $ sort $ getIngameAdvancements g1 $ clab
           clab = csID cs0
           g1 = makeGraph  g0 schema res1

makeCharGen :: G.RDFGraph  -- ^ Schema graph
           -> G.RDFGraph  -- ^ Resource graph
           -> G.RDFGraph  -- ^ Raw character graph
           -> CharGen         -- ^ Resulting datastructure
makeCharGen schema res1 g0 = CharGen 
             { charID = clab
             , charName = ""
             , charGraph = g1
             , rawGraph = g0
             , baseSheet = cs0
             , charSheets = makeCS schema as "Game Start" cs0
             }
     where cs0 = getInitialCharacter char
           char = fromRDFGraph g0 clab
           clab = head $ characterFromGraph g0
           g1 = makeGraph  g0 schema res1
           as = reverse $ sort $ getIngameAdvancements g1 $ clab

makeCS :: RDFGraph -> [Advancement] -> String -> CharacterSheet -> [CharStage] 
makeCS schema  as stage0 cs0 = makeCS' schema as [stage]
   where stage = CharStage 
                   { stage = stage0
                   , advancement = Nothing
                   , sheetObject = cs0
                   , sheetGraph = makeCGraph schema cs0 }
makeCS' :: RDFGraph -> [Advancement] -> [CharStage] -> [CharStage]
makeCS' schema [] xs = xs
makeCS' schema (a:as) xs = makeCS' schema as (y:xs)
   where y = CharStage 
                   { stage = advLabel a
                   , advancement = Just a
                   , sheetObject = cs
                   , sheetGraph = makeCGraph schema cs }
         cs = advanceCharacter (sheetObject $ head xs) a

makeCGraph schema = R.prepareRecord schema . makeRDFGraph

getGameStartCharacter :: G.RDFGraph -> G.RDFLabel -> Maybe CharacterSheet
getGameStartCharacter g label = Just $ getGameStartCS g y
     where x = fromRDFGraph g label :: Character
           y = getInitialCharacter x

-- | Get initial CharacterSheet, before *any* advancements.
getInitialCharacter ::
    Character          -- ^ Character Object
    -> CharacterSheet  -- ^ Empty charactersheet (age 0) for the character
getInitialCharacter c = defaultCS {
            csID = characterID c,
            born = getIntProperty (armRes "hasBirthYear") $ characterData c,
            csMetadata = characterData  c
         }

getGameStartCS :: G.RDFGraph -> CharacterSheet -> CharacterSheet
getGameStartCS g cs = foldl advanceCharacter cs as
    where as = sort $ getPregameAdvancements g $ csID cs

-- | Given a graph and a string identifying a character
-- make a list of all ingame character sheets for the 
-- character by applying all available advancements.
getAllCS :: G.RDFGraph -> G.RDFLabel -> Maybe [CharacterSheet]
getAllCS g c | cs == Nothing = Nothing
             | otherwise     = Just $ cs':advanceList cs' as
    where cs = getGameStartCharacter g c
          cs' = fromJust cs
          as = sort $ getIngameAdvancements g c

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
        , csYear = y
        , csSeason = s
        , csTraits = advanceTraitList (csTraits cs) (sort $ traits adv)
        , csItems = advanceTraitList (csItems cs) (items adv)
     }
     where (s,y) = maybeNextSeason $ (season adv, year adv)



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

-- |
-- = Instances - Load Character object from graph

instance FromRDFGraph Character where
   fromRDFGraph g label = defaultCharacter {
                 characterID = label,
                 characterData = getCharacterMetadata g label
                 }
