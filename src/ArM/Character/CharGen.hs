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
module ArM.Character.CharGen where

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe
import ArM.KeyPair
import ArM.Resources
import ArM.BlankNode
import ArM.Rules (makeGraph)
import ArM.Rules.Aux
import ArM.Character.Character
import ArM.Character.Advancement
import ArM.Types.Character
import ArM.Types.Saga
import ArM.Types.Season
import Data.List (sort,sortBy)

import Debug.Trace
ttrace x = trace (show x) x
strace x = trace  x x

-- ^ A `CharStage` object represents a character's state of development
-- at one particular point on the in-game timeline. 
data CharStage = CharStage 
     { advancement :: Advancement  
       -- ^ The advancement of leading to the stage
     , sheetObject :: CharacterSheet     
       -- ^ The resulting character sheet
     , sheetGraph :: RDFGraph 
       -- ^ The character sheet as an RDF Graph
     }  deriving (Eq,Show)

findSeason :: [CharStage] -> CharTime -> Maybe CharStage
findSeason [] _ = Nothing
findSeason (x:xs) t | ttrace (timeOf x) == ttrace t = trace ("findSeason returns Just " ++ show x) $ Just x
                    | ttrace (timeOf x) > ttrace t = trace ("findSeason returns Nothing") Nothing
                    | otherwise  = trace "findSeason recurses" findSeason xs t

putAdvancement :: CharGen -> Advancement -> CharGen
putAdvancement cg adv = cg { charSheets = putSeason cs0 csl adv }
           where cs0 = baseSheet cg
                 csl = charSheets cg

putSeason :: CharacterSheet
          -> [CharStage] 
          -> Advancement 
          -> [CharStage] 
putSeason cs [] a =  [makeCharStage cs a]
putSeason cs (x:xs) a 
                 | timeOf a < timeOf x = x:makeCharStage (f xs) a:xs
                 | timeOf a == timeOf x = makeCharStage (f xs) a:xs
                 | otherwise  = x:putSeason cs xs a
             where f [] = cs
                   f (y:ys) = sheetObject y

makeCharStage :: CharacterSheet -> Advancement -> CharStage
makeCharStage cs adv = CharStage 
              { advancement = adv
              , sheetObject = advanceCharacter cs adv 
              , sheetGraph = emptyGraph }
             

-- ^ A `CharGen` object represents a character's development over a
-- series of stages.  It contains a list of CharStage objects which
-- in turn contains the Character Sheet for each point in time, as
-- well as the raw data used in calculation and the character name
-- for display purposes.
data CharGen = CharGen 
      { charID :: RDFLabel      -- ^ Character ID 
      , charName :: String      -- ^ Character Name (for display purpose)
      , rawGraph :: RDFGraph    -- ^ Raw graph as stored on file
      , charGraph :: RDFGraph   -- ^ Augmented graph with inference
      , baseSheet :: CharacterSheet 
        -- ^ Character Sheet at the start of the process
      , charSheets :: [CharStage]  
        -- ^ List of development stages, most recent first
      }  deriving (Eq)

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

getAdvFiles ft s = getFiles "hasAdvancementFile"

-- |
-- = Instances of standard classes

instance Show CharGen where
    show cs = charName cs ++ " (" ++ (show $ charID cs) ++ ")"
instance HasTime CharStage where
    timeOf = timeOf . sheetObject

-- |
-- = Making Character Sheets
makeCharGen :: G.RDFGraph  -- ^ Schema graph
           -> G.RDFGraph  -- ^ Resource graph
           -> G.RDFGraph  -- ^ Raw character graph
           -> CharGen         -- ^ Resulting datastructure
makeCharGen schema res1 g0 = trace ("makeCharGen " ++ show clab) $ CharGen 
             { charID = clab
             , charName = ""
             , charGraph = g1
             , rawGraph = g0
             , baseSheet = cs0
             , charSheets = makeCS schema as cs0
             }
     where as = trace ( "makeCharGen " ++ show clab) $ sortBy (flip compare) $ getAllAdvancements g1 $ clab
           cs0 = getInitialCS g1
           clab = ttrace $ csID cs0
           g1 = makeGraph  g0 schema res1

makeCS :: RDFGraph -> [Advancement] -> CharacterSheet -> [CharStage] 
makeCS schema [] cs0 = []
makeCS schema (a:as) cs0 = trace "makeCS" $ makeCS' schema as [y]
   where y = CharStage 
                   { advancement = a
                   , sheetObject = cs
                   , sheetGraph = makeCGraph schema cs }
         cs = advanceCharacter cs0 a
makeCS' :: RDFGraph -> [Advancement] 
        -> [CharStage] -- ^ CharStages already constructed
        -> [CharStage]
makeCS' schema [] xs = xs
makeCS' schema (a:as) xs = trace "makeCS'" $ makeCS' schema as (y:xs)
   where y = CharStage 
                   { advancement = a
                   , sheetObject = cs
                   , sheetGraph = makeCGraph schema cs }
         cs = advanceCharacter (sheetObject $ head xs) a
