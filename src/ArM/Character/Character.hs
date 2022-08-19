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
-----------------------------------------------------------------------------
module ArM.Character.Character ( CharacterSheet(..)
                               , getGameStartCharacter
                               , getAllCS
                               , ToRDFGraph(..)
                               ) where

import qualified Swish.RDF.Graph as G
import           Swish.VarBinding  (vbMap)
import           Data.Maybe (fromJust)
import           Data.List (sort)
import ArM.Resources
import ArM.Character.Trait
import ArM.Character.Advancement
import ArM.KeyPair
import qualified ArM.Character.Metadata as CM
import ArM.Types.Character

-- import Debug.Trace
trace x y = y

getCharacterMetadata = CM.getCharacterMetadata


getGameStartCharacter :: G.RDFGraph -> G.RDFLabel -> Maybe CharacterSheet
getGameStartCharacter g label = Just $ getGameStartCS g y
     where x = CM.fromRDFGraph g label :: Character
           y = getInitialCharacter x

-- | get initial CharacterSheet from an RDFGraph
getInitialCharacter :: Character -> CharacterSheet
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
        , csTraits = advanceTraitList (sort $ csTraits cs) (sort $ traits adv)
        , csItems = advanceTraitList (csItems cs) (items adv)
     }
     where (s,y) = maybeNextSeason $ (season adv, year adv)

maybeNextSeason :: (String,Maybe Int) ->  (String,Maybe Int)
maybeNextSeason ("",Just y) = ("",Just (y+1)) 
maybeNextSeason ("",Nothing) = ("",Nothing) 
maybeNextSeason (s,Just y) = (s',Just y') where (s',y') = nextSeason (s,y)
maybeNextSeason (s,Nothing) = (s',Nothing) where (s',y') = nextSeason (s,0) 
