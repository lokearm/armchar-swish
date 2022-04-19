-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Auxiliary Functions to handle queries
--
-----------------------------------------------------------------------------
module ArM.Character where

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
import ArM.Advancement
import ArM.Query
import ArM.Metadata

data Character = Character {
         characterID :: String,
         initialSheetID :: String,
         pregameAdv :: [Advancement],
         ingameAdv :: [Advancement],
         csTraits :: [Trait],
         metadata :: [Triple]
       }  deriving (Eq,Show)
defaultCharacter = Character {
         characterID = "",
         initialSheetID = "",
         pregameAdv = [],
         ingameAdv = [],
         csTraits = [],
         metadata = []
       }  

data Trait = Trait {
          traitID :: String
       }  deriving (Eq,Show)
     
-- | get a Character from an RDFGraph
getCharacter :: RDFGraph -> String -> Character
getCharacter g c = defaultCharacter {
           characterID = c,
           initialSheetID = cs,
           pregameAdv = sort $ getPregameAdvancements g c,
           ingameAdv = sort $ getIngameAdvancements g c,
           csTraits = [],
           metadata = getCharacterMetadata g cs
         }
         where cs = show $ fromJust $ getInitialSheet g c

-- | Given an identifier for the character, find the identifier for
-- the initial character sheet
getInitialSheet :: RDFGraph -> String -> Maybe RDFLabel
getInitialSheet g c = vbMap vb (G.Var "s")
    where 
      vb = head $ rdfQueryFind q g
      q = qparse $ prefixes ++ c
        ++ " <https://hg.schaathun.net/armchar/schema#hasInitialSheet> ?s . " 

