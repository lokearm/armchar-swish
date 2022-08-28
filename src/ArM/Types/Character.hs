{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle Characters and Traits, with some basic associated functions.
--
-----------------------------------------------------------------------------
module ArM.Types.Character where

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe
import ArM.Types.Season
import ArM.KeyPair
import ArM.Resources
import ArM.BlankNode
import ArM.Rules.Aux
import ArM.Types.RDF
-- import ArM.Types.Advancement
import ArM.Types.Trait
import Data.Aeson
import Data.Aeson.Key

import ArM.Trace

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

-- | 
-- = Character Sheet

data CharacterSheet = CharacterSheet {
      csID :: RDFLabel,
      -- ^ Character ID (i.e. same ID for every season)
      sheetID :: Maybe RDFLabel,  
      -- ^ ID of the Character Sheet, usually Nothing suggesting a blank node
      csTime :: CharTime,  -- ^ Current Year
      born     :: Int,      -- ^ Year of Birth
      csItems :: [Trait],    -- ^ List of possessions (weapons, equipment)
      csTraits :: [Trait],  -- ^ List of traits (abilities, spells, etc.)
      csMetadata :: KeyPairList
      -- ^ Metadata, i.e. data which are not traits or items.
      }  deriving (Eq)
instance Show CharacterSheet where
    show cs = "**" ++ show (csID cs) ++ "**\n" 
           ++ "-- " ++ ( showSheetID ) cs ++ "\n"
           ++ "Traits:\n" ++ showw ( csTraits cs )
           ++ "Metadata Triples:\n" ++ show ( csMetadata cs )
        where showw [] = ""
              showw (x:xs) = "  " ++ show x ++ "\n" ++ showw xs
defaultCS :: CharacterSheet
defaultCS = CharacterSheet {
         csID = noSuchCharacter,
         sheetID = Nothing,
         csTime = defaultCharTime,
         born = 0,
         csItems = [],
         csTraits = [],
         csMetadata = KeyPairList []
       }  

showSheetID :: CharacterSheet -> String
showSheetID = f . sheetID
    where f Nothing = "no sheet ID"
          f (Just x) = show x


instance ToRDFGraph CharacterSheet where
   makeRDFGraph cs = trace "makeRDFGraph for CharacterSheet" $
         ( listToRDFGraph  . fst . runBlank ( csToArcListM cs' ) )
         ("charsheet",1)
      where cs' = cs { csMetadata = KeyPairList $ a xs }
            KeyPairList xs = csMetadata cs
            aAge x ys  
                   | x == 0 = ys
                   | otherwise = KeyValuePair (armRes "hasAge") (litInt x):ys
            aYear Nothing ys   = ys
            aYear (Just x) ys  = KeyValuePair (armRes "inYear") (litInt x):ys
            aSeason x ys  
                   | x == "" = ys
                   | otherwise = KeyValuePair (armRes "atSeason") (litString x):ys
            age' Nothing _ = 0
            age' (Just y) b | b == 0 = 0 
                            | otherwise = y - b
            age = age' (hasYear cs) (born cs)
            a = aAge age . aYear (hasYear cs) . aSeason (hasSeason cs)
instance ToRDFGraph Character where
   makeRDFGraph cs = listToRDFGraph  ( ct:ms )
       where x = characterID cs
             ms = keyvalueToArcList x (fromKeyPairList $ characterData cs)
             ct = arc x typeRes (armRes  "Character")

csToArcListM :: CharacterSheet -> BlankState [RDFTriple]
csToArcListM cs = do
          x <- getSheetIDM $ sheetID cs
          tsm <- fixBlanksM $ csTraits cs
          ism <- fixBlanksM $ csItems cs
          let ht = map ( \ y -> arc x (armRes "hasTrait") (fromJust $ traitID y) ) tsm
          let hi = map ( \ y -> arc x (armRes "hasPossession") (fromJust $ traitID y) ) ism
          let ts =  map traitContents tsm
          let is =  map traitContents ism
          let metadata = keyvalueToArcList x (fromKeyPairList $ csMetadata cs)
          let ms = metadata ++ hi ++ ht 
          let ct = arc x isCharacterLabel (csID cs)
          let ct1 = arc x typeRes csRes 
          let ms1 = foldr (++) ms ts
          return $ ct1:ct:foldr (++) ms1 is

fixBlanksM :: [Trait] -> BlankState [Trait]
fixBlanksM [] = return []
fixBlanksM (x:xs) = do
             x' <- fixBlankNodeM x
             xs' <- fixBlanksM xs
             return $ x':xs'
fixBlankNodeM :: Trait -> BlankState Trait
fixBlankNodeM t 
   | traitContents t == [] = return t
   | key /= (armRes "unnamedBlankNode") = return $ trace ("Non-blank node "++show key) t
   | otherwise = do
        b <- getBlank
        return $ trace ("BlankNode "++show b) $ t { traitContents = map ( replaceBlank b ) 
                      $ traitContents t }
     where key = ttrace $ arcSubj $ head $ traitContents t

replaceBlank :: RDFLabel -> RDFTriple -> RDFTriple
replaceBlank b x =  arc b ( arcPred x ) ( arcObj x )
            
getSheetIDM :: Maybe RDFLabel -> BlankState RDFLabel
getSheetIDM Nothing = getBlank
getSheetIDM (Just x) = return x

-- |
-- = CharacterSheet

instance ToJSON CharacterSheet where 
    toJSON cs = object (c:x:xs)
       where x = (fromString "arm:hasTrait") .= (toJSON (csTraits cs))
             xs = map tripleToJSON (fromKeyPairList $ csMetadata cs)
             c = (fromString "arm:isCharacter") .= (show $ csID cs)

instance ToJSON Character where 
    toJSON c = toJSON $ p x xs
        where x = KeyValuePair (armRes "isCharacter") $ characterID c
              xs = characterData c 
              p y (KeyPairList ys) = KeyPairList (y:ys) 

instance FromJSON Character where 
    parseJSON val = fmap kpToChar $ parseJSON val

-- | Auxiliary to parseJSON Character
kpToChar :: KeyPairList -> Character
kpToChar (KeyPairList xs) = defaultCharacter {
         characterID = fromJ $ getProperty (armRes "isCharacter") xs,
         characterData = KeyPairList xs
         }
         where fromJ Nothing = noSuchCharacter
               fromJ (Just x) = x

instance HasTime CharacterSheet where
    timeOf = csTime

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
