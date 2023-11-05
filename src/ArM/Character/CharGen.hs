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
                             , makeCharGen
                             , findSeason
                             , putCharacter
                             , putAdvancement ) where

import Swish.RDF.Graph as G
import ArM.KeyPair()
import ArM.Rules (makeGraph)
import ArM.Types.Character
import ArM.Types.Season
import ArM.Types.RDF
import ArM.Types.Advancement
import qualified ArM.Rules.Record as R
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
     , sheetGraph :: RDFGraph 
       -- ^ The character sheet as an RDF Graph
     }  deriving (Eq,Show)

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

-- |
-- = Search and Insert

-- | Find the Character Stage at a given time.
findSeason :: [CharStage] -> CharTime -> Maybe CharStage
findSeason [] _ = Nothing
findSeason (x:xs) t | timeOf x == t = Just x
                    | timeOf x < t = Nothing
                    | otherwise  = findSeason xs t

-- | Insert a new advancement object.  If an advancement
-- already exists at the same time, it will be replaced.
putAdvancement :: RDFGraph  -- ^ Schema Graph
               -> RDFGraph  -- ^ Resource Graph
               -> CharGen -> Advancement -> CharGen
putAdvancement schema res1 cg adv = trace "TCG.putAdvancement" 
     $ updateBaseGraph schema res1
     $ cg { charSheets = trace "call putSeason" csl1 }
       where cs0 = baseSheet cg
             csl = charSheets cg
             csl1 = putSeason schema cs0 csl adv 

updateBaseGraph :: RDFGraph -> RDFGraph -> CharGen -> CharGen
updateBaseGraph schema res1 cg = cg { rawGraph = g
                                    , charGraph = g1
                                    , baseGraph = bg
                                    , baseSheet = getInitialCS g1 }
       where g = foldl addGraphs (baseGraph cg) 
               $ map ( makeRDFGraph . advancement ) $ charSheets cg
             g1 = makeGraph  g schema res1 
             bg = trace ("charID "++(show$charID cg))
                  extractBaseCharacterGraph g1 $ charID cg

putCharacter :: RDFGraph   -- ^ Schema Graph
             -> RDFGraph   -- ^ Resource Graph
             -> CharGen    -- ^ Old CharGen object
             -> RDFGraph   -- ^ New graph of Character Metadata
             -> CharGen    -- ^ Updated CharGen object
putCharacter schema res1 cg chgraph = cg1 { charSheets = csl1 }
       where cg1 = updateBaseGraph schema res1 $ cg { baseGraph = chgraph }
             csl = charSheets cg
             cs0 = baseSheet cg1
             csl1 = trace ("csl1 " ++ (show $ length csl1')) 
                  $ trace ("as " ++ (show $ length as))
                  $ csl1'
             csl1' = makeCS schema as cs0 
             as = reverse $ map advancement csl

-- | Insert a new advancement object into a list of CharStage objects.
-- This is an auxiliary to `putAdvancement`.
putSeason :: RDFGraph
          -> CharacterSheet
          -> [CharStage] 
          -> Advancement 
          -> [CharStage] 
putSeason schema cs [] a = [makeCharStage schema cs a]
putSeason schema cs (x:xs) a 
                 | atime > xtime =  trace "putSeason >" $ y:x:xs
                 | atime == xtime =  trace "putSeason =" $ y':xs
                 | otherwise  = trace "putSeason otherwise" $ x':xs'
        where f [] = cs
              f (z:_) = trace ("get SheetObject from head" 
                        ++ (show $ timeOf $ sheetObject z)) sheetObject z
              y = trace ("y = makeCharStage " ++ show atime ++ show xtime) 
                $ makeCharStage schema (sheetObject x) a
              y' = trace ("y' = makeCharStage " ++ show atime ++ show xtime) 
                $ makeCharStage schema (f xs) a
              atime = timeOf a
              xtime = timeOf $ advancement x
              xs' = putSeason schema cs xs a
              x' = trace ("x' = makeCharStage " ++ show atime ++ show xtime) 
                 $ makeCharStage schema (f xs') (advancement x)

-- | Make a CharStage object by applying a given Advancement to a
-- given CharacterSheet
makeCharStage :: RDFGraph -- ^ Shema Graph
              -> CharacterSheet -> Advancement -> CharStage
makeCharStage schema cs0 adv = CharStage 
              { advancement = adv
              , sheetObject = cs
              , sheetGraph = makeCGraph schema cs }
              where cs = advanceCharacter cs0 adv 
             
makeCGraph :: RDFGraph -> CharacterSheet -> RDFGraph
makeCGraph schema = R.prepareRecord schema . makeRDFGraph


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
     where as = trace ( "makeCharGen (adv)" ++ show clab) $ sort $ getAllAdvancements g1 $ clab
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
