{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.CharGen
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle characters as stored in web server memory.
--
-----------------------------------------------------------------------------
module ArM.Character.CharGen ( CharGen(..)
                             , CharStage(..)
                             , sheetGraph
                             , makeCharGen
                             , findSeason
                             ) where

import Swish.RDF.Graph as G
import ArM.KeyPair()
import ArM.Rules (makeGraph)
import ArM.Types.Character
import ArM.Types.Season
import ArM.Types.RDF
import ArM.Types.Advancement
import ArM.Rules.Record (prepareRecord)
import ArM.Rules.SheetCalculation (calculateSheet)
import Data.List (sort)

import ArM.Debug.NoTrace

-- |
-- = Data Types

-- ^ A `CharStage` object represents a character's state of development
-- at one particular point on the in-game timeline. 
data CharStage = CharStage 
     { advancement :: Advancement  
       -- ^ The advancement leading to the stage
     , sheetObject :: CharacterSheet     
       -- ^ The resulting character sheet
     , sheetRawGraph :: RDFGraph 
       -- ^ The character sheet as an RDF Graph
     }  deriving (Eq,Show)

sheetGraph :: CharStage -> RDFGraph
sheetGraph = calculateSheet . sheetRawGraph

-- | A `CharGen` object represents a character's development over a
-- series of stages.  It contains a list of CharStage objects which
-- in turn contains the Character Sheet for each point in time, as
-- well as the raw data used in calculation and the character name
-- for display purposes.
data CharGen = CharGen 
      { charID :: RDFLabel      -- ^ Character ID 
      , charName :: String      -- ^ Character Name (for display purpose)
      , rawGraph :: RDFGraph    -- ^ Raw graph as stored on file
      , charGraph :: RDFGraph   -- ^ Augmented graph with inference
      , baseGraph :: RDFGraph   -- ^ Graph containing only the character 
      , baseSheet :: CharacterSheet 
        -- ^ Character Sheet at the start of the process
      , charSheets :: [CharStage]  
        -- ^ List of development stages, most recent first
      }  deriving (Eq)

-- | Find the Character Stage at a given time.
findSeason :: [CharStage] -> CharTime -> Maybe CharStage
findSeason [] _ = Nothing
findSeason (x:xs) t | timeOf x == t = Just x
                    | timeOf x < t = Nothing
                    | otherwise  = findSeason xs t


-- | Make a CharStage object by applying a given Advancement to a
-- given CharacterSheet
makeCharStage :: RDFGraph -- ^ Shema Graph
              -> CharacterSheet -> Advancement -> CharStage
makeCharStage schema cs0 adv = CharStage 
              { advancement = adv
              , sheetObject = cs
              , sheetRawGraph = makeCGraph schema cs }
              where cs = advanceCharacter cs0 adv 
             
makeCGraph :: RDFGraph -> CharacterSheet -> RDFGraph
makeCGraph schema = prepareRecord schema . makeRDFGraph


-- |
-- = Keys 

-- The `CharacterKey` type is used to index character sheets and
-- `CharStage` objects when these are stored in maps.
data CharacterKey = CharacterKey {
            keyYear :: Int,
            keySeason :: String,
            keyChar :: String } deriving (Ord,Eq,Show)

class Keyable a where
    getKey :: a -> CharacterKey
instance Keyable CharacterSheet where
   getKey cs = CharacterKey { keyYear = case (hasYear cs) of
                                Nothing -> 0
                                (Just y) -> y,
                           keySeason = (hasSeason cs),
                           keyChar = show $ csID cs }
instance Keyable CharStage where
   getKey = getKey . sheetObject

-- |
-- = Instances of standard classes

instance Show CharGen where
    show cs = charName cs ++ " (" ++ (show $ charID cs) ++ ")\n" ++ showw (charSheets cs)
         where showw xs = show $ map timeOf xs
instance HasTime CharStage where
    timeOf = timeOf . sheetObject

-- |
-- = Making Character Sheets

makeCharGen :: G.RDFGraph  -- ^ Schema graph
           -> G.RDFGraph   -- ^ Resource graph
           -> G.RDFGraph   -- ^ Raw character graph
           -> CharGen      -- ^ Resulting datastructure
makeCharGen schema res1 g0 = trace ("makeCharGen " ++ show clab) $ CharGen 
             { charID = clab
             , charName = ""
             , charGraph = g1
             , rawGraph = g0
             , baseGraph = extractBaseCharacterGraph g0 clab
             , baseSheet = cs0
             , charSheets = makeCS schema as cs0
             }
     where as = sort $ getAllAdvancements g1 $ clab
           cs0 = getInitialCS g1
           clab = csID cs0
           g1 = makeGraph  g0 schema res1

makeCS :: RDFGraph -> [Advancement] -> CharacterSheet -> [CharStage] 
makeCS _ [] _ = []
makeCS schema (a:as) cs0 = makeCS' schema as [y]
   where y = makeCharStage schema cs0 a
makeCS' :: RDFGraph -> [Advancement] 
        -> [CharStage] -- ^ CharStages already constructed
        -> [CharStage]
makeCS' _ [] xs = xs
makeCS' schema (a:as) xs = makeCS' schema as (y:xs)
   where y = makeCharStage  schema (sheetObject $ head xs) a
