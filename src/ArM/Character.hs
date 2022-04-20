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

data CharacterSheet = CharacterSheet {
         csID :: String,
         sheetID :: Maybe String,
         csTraits :: [Trait],
         csMetadata :: [Triple]
       }  deriving (Eq)
instance Show CharacterSheet where
    show cs = "**" ++ csID cs ++ "**\n" 
           ++ "-- " ++ fromJust ( sheetID cs ) ++ "\n"
           ++ "Traits:\n" ++ showw ( csTraits cs )
           ++ "Metadata Triples:\n" ++ showw ( csMetadata cs )
        where showw [] = ""
              showw (x:xs) = "  " ++ show x ++ "\n" ++ showw xs
defaultCS = CharacterSheet {
         csID = "",
         sheetID = Nothing,
         csTraits = [],
         csMetadata = []
       }  

-- | apply a given Advancement to a given CharacterSheet
advanceCharacter :: CharacterSheet -> Advancement -> CharacterSheet 
advanceCharacter cs adv = cs

-- | apply a given TraitAdvancement to a given Trait
advanceTrait :: Trait -> TraitAdvancement -> Trait 
advanceTrait trait adv = trait

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

-- | get initial CharacterSheet from an RDFGraph
getInitialCS :: RDFGraph -> String -> CharacterSheet
getInitialCS g = fixCS g . getInitialCS' g

getInitialCS' :: RDFGraph -> String -> CharacterSheet
getInitialCS' g c = defaultCS {
            csID = c,
            sheetID = Just cs, 
            csTraits = [],
            csMetadata = getCharacterMetadata g cs
         }
         where cs = show $ fromJust $ getInitialSheet g c
     
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

-- | Given an identifier for the character, find the identifier for
-- the initial character sheet
getInitialSheet :: RDFGraph -> String -> Maybe RDFLabel
getInitialSheet g c = vbMap vb (G.Var "s")
    where 
      vb = head $ rdfQueryFind q g
      q = qparse $ prefixes ++ c
        ++ " <https://hg.schaathun.net/armchar/schema#hasInitialSheet> ?s . " 




-- | Trait Resource
data Trait = Trait {
    traitID :: Maybe RDFLabel,
    traitClass :: Maybe String,
    traitContents :: [Triple]
   } deriving (Eq)

-- | Make a Trait object from a list of Quads
toTrait :: [Quad] -> Trait
toTrait [] = Trait {
         traitID = Nothing,
         traitClass = Nothing,
         traitContents = [] }
toTrait xs = Trait { 
         traitID = Just $ qfst $ head xs,
         traitClass = getTraitClass ys,
         traitContents = ys }
         where ys = toTripleList xs 

instance Show Trait where
   show a = "**" ++ y (traitID a) ++ " " ++ s (traitClass a) ++ "**\n" 
                 ++ sc (traitContents a) 
                 ++ "\n"
      where 
         y Nothing = ""
         y (Just x) = show x
         s Nothing = ""
         s (Just x) = x
         sc [] = ""
         sc ((_,x,y):xs) = "  " ++ x ++ ": " ++ y ++ "\n" ++ sc xs
instance Ord Trait where
   compare x y | traitClass x < traitClass y = LT
               | traitClass x > traitClass y = GT
               | traitID x < traitID y = LT
               | traitID x > traitID y = GT
               | otherwise = EQ

-- | Auxiliary for 'getInitialSheet'
fixCS :: RDFGraph -> CharacterSheet -> CharacterSheet
fixCS g a = a { csTraits = sort $ getTraits q g }
    where cqt' = cqt . show . fromJust . sheetID 
          q = cqt' a
          getTraits q g = map toTrait 
               $ quadSplit $ map quadFromBinding $ rdfQueryFind q g 

-- | Query Graph to get traits for CharacterSheet
cqt :: String -> RDFGraph
cqt s = qparse $ prefixes 
      ++ s ++ " <https://hg.schaathun.net/armchar/schema#Trait> ?id . " 
      ++ "?id ?property ?value . "
      ++ "?property rdfs:label ?label . "
