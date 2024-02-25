{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Update
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Function to update the character graphs. 
-- This is not used for the CLI project, but includes functions critical
-- to the server.
--
-----------------------------------------------------------------------------
module ArM.Character.Update ( CharGen(..)
                             , CharStage(..)
                             , sheetGraph
                             , makeCharGen
                             , findSeason
                             , putCharacter
                             , putAdvancement ) where

import ArM.Character.CharGen

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
