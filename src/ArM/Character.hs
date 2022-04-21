-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- The CharacterSheet data type and corresponding functions
--
-----------------------------------------------------------------------------
module ArM.Character where

import Data.Set (fromList)
import Swish.RDF.Parser.N3 (parseN3fromText)
import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import qualified Data.Text.Lazy as T
import Swish.RDF.VarBinding as VB 
import Network.URI (URI,parseURI)
import Swish.VarBinding  (vbMap)
import Data.Maybe
import Data.List (sort)
import ArM.Resources
import ArM.Internal.Trait
import ArM.Advancement
import ArM.Query
import ArM.Metadata

data CharacterSheet = CharacterSheet {
         csID :: String,
         sheetID :: Maybe RDFLabel,
         csTraits :: [Trait],
         csMetadata :: [Triple]
       }  deriving (Eq)
instance Show CharacterSheet where
    show cs = "**" ++ csID cs ++ "**\n" 
           ++ "-- " ++ ( show . fromJust . sheetID ) cs ++ "\n"
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
advanceCharacter cs adv = cs { 
     sheetID = Nothing,
     csTraits = advanceTraitList (csTraits cs) (traits adv)
     }


-- | get initial CharacterSheet from an RDFGraph
getInitialCS :: RDFGraph -> String -> CharacterSheet
getInitialCS g = fixCS g . getInitialCS' g

getInitialCS' :: RDFGraph -> String -> CharacterSheet
getInitialCS' g c = defaultCS {
            csID = c,
            sheetID = x,
            csTraits = [],
            csMetadata = getCharacterMetadata g cs
         }
         where cs = show $ fromJust $ x
               x = getInitialSheet g c
     

-- | Given an identifier for the character, find the identifier for
-- the initial character sheet
getInitialSheet :: RDFGraph -> String -> Maybe RDFLabel
getInitialSheet g c = vbMap vb (G.Var "s")
    where 
      vb = head $ rdfQueryFind q g
      q = qparse $ prefixes ++ c
        ++ " <https://hg.schaathun.net/armchar/schema#hasInitialSheet> ?s . " 


-- | Auxiliary for 'getInitialSheet'
fixCS :: RDFGraph -> CharacterSheet -> CharacterSheet
fixCS g a = a { csTraits = sort $ getTraits a g }

-- | Get a list of traits. Auxiliary function for 'fixCS'.
getTraits :: CharacterSheet -> RDFGraph -> [Trait]
getTraits a g = map toTrait 
               $ quadSplit $ map quadFromBinding $ rdfQueryFind q g 
    where q = cqt $ show $ fromJust $ sheetID a

-- | Query Graph to get traits for CharacterSheet
cqt :: String -> RDFGraph
cqt s = qparse $ prefixes 
      ++ s ++ " <https://hg.schaathun.net/armchar/schema#hasTrait> ?id . " 
      ++ "?id ?property ?value . "
      ++ "?property rdfs:label ?label . "

         -- csID :: String,
         -- sheetID :: Maybe String,
         -- csTraits :: [Trait],
         -- csMetadata :: [Triple]
         --

csToRDFGraph :: CharacterSheet -> RDFGraph
csToRDFGraph = toRDFGraph . csToArcSet
csToArcSet :: CharacterSheet -> RDFArcSet
csToArcSet = fromList . csToArcList
csToArcList :: CharacterSheet -> [RDFTriple]
csToArcList cs = ct:foldl (++) ms ts
    where ts = map traitToArcList (csTraits cs)
          ms = triplesToArcList x (csMetadata cs)
          x = getSheetID cs $ sheetID cs
          ct = arc charlabel isCharacterLabel x
          charlabel = getCSID cs
getCSID :: CharacterSheet -> RDFLabel
getCSID = toRDFLabel . fromJust . parseURI . csID 

getLabel :: Show a => Maybe a -> String
getLabel Nothing = "nn"
getLabel (Just x) = show x

getSheetID cs Nothing = Blank "TODO"
   -- where  cid = fromRDFLabel $ fromJust $ csID cid
getSheetID _ (Just x) = x

