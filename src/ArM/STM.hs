-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.STM
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- This defines the data model as managed in memory, using
-- Software Transactional Memory (STM).
-- The `getSTM` functions gives a simple call to set up the data model
-- using three RDF graphs which can be read from file (`ArM.Load` module).
-- A lot of inferred graphs and other objects are set up as part of
-- the data models, including the Character Map which supports the 
-- `lookup` function.
--
-- The current version only stores a single character in the STM state.
-- Several representations are available.
-- 1.  `charRawGreph` is the RDF Graph as stored on file.
-- 2.  `charGreph` is the augmented graph, incorporating the schema,
--     the resources, and additional inference.
-- 3.  `characterMap` contains a map from (character,year,season)
--     to character sheets at each point in time.  Each character sheet
--     is stored as an RDF graph.
--
-- The last two are computed from `charRawGraph` and must never be  
-- edited other than indirectly via the raw graph.
-- For reading, character and advancements should be looked up in
-- `charGraph` while character sheets should be looked up in the
-- `characterMap`.
--
-- It may also be important to look up and add resources.  Similarly
-- to the above, editing should be confined to `resourceRawGraph`
-- while reading should use `resourceGraph` which is derived from the raw
-- graph,
--
-----------------------------------------------------------------------------
module ArM.STM ( ArM.STM.lookup
               , CM.CharacterRecord(..)
               , getState
               , getStateGraph
               , getSchemaGraph
               , getResourceGraph
               , putAdvancement
               , putCharacter
               , updateGraph -- TEST
               , MapState(..)
               ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad.IO.Class (liftIO)
import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import           Data.Maybe (fromJust)
import           Network.URI (URI)

import qualified ArM.STM.CharacterMap as CM
import qualified ArM.Character as C
import qualified ArM.Types.Character as TC
import qualified ArM.Resources as AR
import           ArM.Rules.Aux
import qualified ArM.Rules.Persistence as RP
import           ArM.Rules (makeGraph, makeGraphs)
import           ArM.Resources

-- | The `MapState` object defines the state of the server.
-- The server process maintains a single `MapState` object in
-- software transactional memory (STM), recording all the data
-- which may potentially change during operation.
data MapState = MapState { charGraph :: G.RDFGraph,
                           schemaGraph :: G.RDFGraph,
                           resourceGraph :: G.RDFGraph,
                           charRawGraph :: G.RDFGraph,
                           schemaRawGraph :: G.RDFGraph,
                           resourceRawGraph :: G.RDFGraph,
                           characterLabel :: G.RDFLabel,
                           characterID :: String,
                           characterMap :: CM.CharacterMap
                           }

-- getSTM res schema g = STM.newTVarIO $ fromJust $ getState res schema g 
-- | Make the State object to be stored in STM.
-- The return value is Either a MapState object or an error message.
getState :: G.RDFGraph -> G.RDFGraph -> G.RDFGraph -> Either MapState String
getState res schema g 
    | cl == Nothing = Right "Failed to make character sheets"
    | cid == Nothing = Right "Could not parse character ID"
    | ll == [] = Right "No character found"
    | tail ll /= [] = Right $ "Multiple characters found\n" ++ show ll
    | otherwise = Left MapState {
                    charGraph = g1,
                    schemaGraph = s1,
                    resourceGraph = res1,
                    charRawGraph = g,
                    schemaRawGraph = schema,
                    resourceRawGraph = res,
                    characterLabel = clab,
                    characterID  = fromJust cid,
                    characterMap = CM.insertListS res1 CM.empty $ fromJust cl
                  }
   where (g1,s1,res1) = makeGraphs (g,schema,res)
         ll = C.characterFromGraph g1
         clab = head ll
         cid = getLocalID clab
         cl = C.getAllCS g1 clab

-- | Replace the raw character graph in the MapState.
-- All other elements are recalculated.
updateGraph :: MapState -> G.RDFGraph -> Either MapState String
updateGraph st g 
    | cl == Nothing = Right "Failed to make character sheets"
    | cid == Nothing = Right "Could not parse character ID"
    | ll == [] = Right "No character found"
    | tail ll /= [] = Right $ "Multiple characters found\n" ++ show ll
    | otherwise = Left st {
                    charGraph = g1,
                    charRawGraph = g,
                    characterLabel = clab,
                    characterID  = fromJust cid,
                    characterMap = CM.insertListS res1 CM.empty $ fromJust cl
                  }
   where g1 = makeGraph  g (schemaGraph st) (resourceGraph st)
         ll = C.characterFromGraph g1
         res1 = resourceGraph st
         clab = head ll
         cid = getLocalID clab
         cl = C.getAllCS g1 clab

-- | Return the state graph (i.e. character data) from STM.
getStateGraph :: STM.TVar MapState -> IO G.RDFGraph
getStateGraph st = fmap charGraph $ STM.readTVarIO st
-- | Return the schema from STM as an RDF Graph.
getSchemaGraph :: STM.TVar MapState -> IO G.RDFGraph
getSchemaGraph st = fmap schemaGraph $ STM.readTVarIO st
-- | Return the resource graph from STM as an RDF Graph.
getResourceGraph :: STM.TVar MapState -> IO G.RDFGraph
getResourceGraph st = fmap resourceGraph $ STM.readTVarIO st


-- | Return the sheet for a given character, season, and year (as RDFGraph).
lookup :: STM.TVar MapState -> String -> String -> Int 
       -> IO (Maybe CM.CharacterRecord)
lookup stateVar char season year = do
          st <- STM.readTVarIO stateVar
          let g = charGraph st
          let res = resourceGraph st
          print $ char ++ " - " ++ season ++ " - " ++ show year
          let cmap =  characterMap st
          print $  AR.armcharRes char
          let charstring = "armchar:" ++ char
          return $ CM.lookup cmap charstring season year


-- getResource :: G.RDFGraph -> G.RDFLabel -> Maybe G.RDFGraph
-- getResource g label = Nothing

-- | Delete `g` and merge `g1` into `g0`.
putGraph :: G.RDFGraph -> G.RDFGraph -> G.RDFGraph -> G.RDFGraph
putGraph g g0 g1 = G.merge (G.delete g0 g) g1

-- | Update the state graph with the given Advancement object.
putAdvancement :: STM.TVar MapState -> TC.Advancement -> IO (Either G.RDFGraph String)
putAdvancement stateVar adv = do 
         STM.atomically $ do
             st <- STM.readTVar stateVar
             let g = charRawGraph st
             let schema = schemaGraph st
             let advg = TC.makeRDFGraph adv
             let g1 = RP.persistGraph schema advg
             let g0 = RP.persistedGraph (charGraph st) (TC.rdfid adv) 
             let gg = (g0 `G.delete` g) `G.addGraphs` g1

             let newst = st `updateGraph` gg
             case (newst) of
                Left s -> do
                   STM.writeTVar stateVar s 
                   return $ Left gg
                Right x -> return $ Right x
-- TODO: Check for conflicting advancements 
--
putCharacter :: STM.TVar MapState -> TC.Character -> IO (Either G.RDFGraph String)
putCharacter stateVar char = do 
         STM.atomically $ do
             st <- STM.readTVar stateVar
             let g = charRawGraph st
             let schema = schemaGraph st
             let charg = TC.makeRDFGraph char
             let g1 = RP.persistChar schema charg
             let g0 = RP.persistedChar (charGraph st) (TC.characterID char) 
             let gg = (g0 `G.delete` g) `G.addGraphs` g1

             let newst = st `updateGraph` gg
             case (newst) of
                Left s -> do
                   STM.writeTVar stateVar s 
                   return $ Left gg
                Right x -> return $ Right x

-- postResource :: STM.TVar MapState -> G.RDFLabel -> G.RDFGraph
             -- -> IO (Maybe G.RDFGraph)
-- postResource stateVar label newres = do 
      -- STM.atomically $ do
          -- st <- STM.readTVarIO stateVar
          -- let g = charGraph st
          -- case (putGraph g label newres) of
             -- Nothing -> return Nothing
             -- (Just gg) -> do
                          -- STM.writeTVar stateVar $ st { charGraph = gg }
                          -- return $ Just gg
