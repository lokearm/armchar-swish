{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- The CharacterSheet data type and corresponding functions
--
-----------------------------------------------------------------------------
module ArM.Character.Character ( CharacterSheet(..)
                               , getGameStartCharacter
                               , getAllCS
                               , ToRDFGraph(..)
                               ) where

import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import qualified Data.Text.Lazy as T
import qualified Swish.RDF.VarBinding as VB 
import           Network.URI (URI,parseURI)
import           Swish.VarBinding  (vbMap)
import Data.Maybe
import           Data.List (sort)
import ArM.Resources
import ArM.Character.Trait
import ArM.Character.Advancement
import ArM.KeyPair
import qualified ArM.Character.Metadata as CM
import           ArM.Rules.Aux (sVar,labelRes,listToRDFGraph,qgraph)
import ArM.Types.Character

getCharacterMetadata = CM.getCharacterMetadata


getGameStartCharacter :: G.RDFGraph -> G.RDFLabel -> Maybe CharacterSheet
getGameStartCharacter g label = Just $ getGameStartCS g y
     where x = CM.fromRDFGraph g label :: Character
           y = getInitialCharacter x
--
-- | get initial CharacterSheet from an RDFGraph
getInitialCharacter :: Character -> CharacterSheet
getInitialCharacter c = defaultCS {
            csID = characterID c,
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
advanceList cs (x:xs) = advanceCharacter cs x : advanceList cs xs

-- | apply a given Advancement to a given CharacterSheet
advanceCharacter :: CharacterSheet -> Advancement -> CharacterSheet 
advanceCharacter cs adv = cs { 
     sheetID = Nothing,
     csYear = year adv,
     csSeason = season adv,
     csTraits = advanceTraitList (csTraits cs) (traits adv)
     }

