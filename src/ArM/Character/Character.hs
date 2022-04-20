-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Auxiliary Functions to handle queries
--
-----------------------------------------------------------------------------
module ArM.Character.Character where

import Swish.RDF.Parser.N3 (parseN3fromText)
import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import qualified Data.Text.Lazy as T
import Swish.RDF.VarBinding as VB 
import Network.URI (URI)
import Swish.VarBinding  (vbMap)
import Data.Maybe
import Data.List (sort)
import ArM.Resources
import ArM.Internal.Trait
import ArM.Advancement
import ArM.Query
import ArM.Metadata

-- | Type ot collect all relevant data about a character.
-- This may be redundant.  CharacterSheet may suffice.
data Character = Character {
         characterID :: String,
         initialSheetID :: String,
         pregameAdv :: [Advancement],
         ingameAdv :: [Advancement],
         characterTraits :: [Trait],
         metadata :: [Triple]
       }  deriving (Eq,Show)
defaultCharacter = Character {
         characterID = "",
         initialSheetID = "",
         pregameAdv = [],
         ingameAdv = [],
         characterTraits = [],
         metadata = []
       }  
-- | get a Character from an RDFGraph
getCharacter :: RDFGraph -> String -> Character
getCharacter g c = defaultCharacter {
           characterID = c,
           initialSheetID = cs,
           pregameAdv = sort $ getPregameAdvancements g c,
           ingameAdv = sort $ getIngameAdvancements g c,
           characterTraits = [],
           metadata = getCharacterMetadata g cs
         }
         where cs = show $ fromJust $ getInitialSheet g c
