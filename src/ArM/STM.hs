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
-- 2.  `charGraph` is the augmented graph, incorporating the schema,
--     the resources, and additional inference.  This is defined
--     by functions from `ArM.Rules.FullGraph`.
-- 3.  `characterMap` contains a map from (character,year,season)
--     to character sheets at each point in time.  Each character sheet
--     is stored as an RDF graph.  The character sheets are calculated
--     by the `getAllCS` function from `ArM.Character.Character` which
--     applies the advancements.
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
               , loadSaga
               , getStateGraph
               , getSchemaGraph
               , getResourceGraph
               , putAdvancement
               , putCharacter
               , putCharGraph 
               , MapState(..)
               ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad.IO.Class (liftIO)
import qualified Swish.RDF.Graph as G
import           Data.Maybe (fromJust)

import qualified ArM.STM.CharacterMap as CM
import qualified ArM.Character as C
import qualified ArM.Types.Character as TC
import qualified ArM.Types.CharGen as TCG
import qualified ArM.Types.Saga as TS
import qualified ArM.Resources as AR
import           ArM.Rules.Aux
import qualified ArM.Rules.Persistence as RP
import qualified ArM.Rules as R
import           ArM.Resources
import           ArM.Load


-- | The `MapState` object defines the state of the server.
-- The server process maintains a single `MapState` object in
-- software transactional memory (STM), recording all the data
-- which may potentially change during operation.
data MapState = MapState { sagaGraph :: STM.TVar G.RDFGraph
                         , charGraph :: STM.TVar [TCG.CharGen]
                         , schemaGraph :: STM.TVar G.RDFGraph
                         , resourceGraph :: STM.TVar G.RDFGraph
                         , charRawGraph :: STM.TVar [G.RDFGraph]
                         , schemaRawGraph :: STM.TVar [G.RDFGraph]
                         , resourceRawGraph :: STM.TVar [G.RDFGraph]
                         , characterID :: STM.TVar String
                         , characterMap :: STM.TVar CM.CharacterMap
                           }

readAllFiles :: [String] -> IO [G.RDFGraph]
readAllFiles = mapM readGraph
mergeGraphs :: [G.RDFGraph] -> G.RDFGraph
mergeGraphs [] = G.emptyGraph
mergeGraphs (x:xs) = foldr G.merge x xs


-- | Make the State object to be stored in STM.
-- The return value is Either a MapState object or an error message.
loadSaga :: String -> IO MapState
loadSaga fn = do
    -- 1. Load Saga
    saga <- readGraph fn
    let sid = head $ TS.sagaFromGraph saga
    sagaVar <- STM.newTVarIO saga
    -- 2. Load Schema
    let schemaFN = TS.getSchemaFiles sid saga
    ss <- readAllFiles schemaFN
    schemaRawVar <- STM.newTVarIO ss
    -- 3. Load resources
    let resFN = TS.getResourceFiles sid saga
    rs <- readAllFiles resFN
    resRawVar <- STM.newTVarIO rs

    -- 4. Augment graphs
    let s0 = mergeGraphs ss
    let res = mergeGraphs rs
    let s1 = R.prepareSchema s0
    let res1 = R.prepareResources $ res `G.merge` s1 

    -- 5. Save augmented graphs
    schemaVar <- STM.newTVarIO s1
    resVar <- STM.newTVarIO res1

    -- 6. Load Character Graphs
    let charFN = TS.getCharacterFiles sid saga
    cs <- readAllFiles charFN

    let cgs = map (C.makeCharGen s1) cs

    charVar <- STM.newTVarIO cgs
    cm <- STM.newTVarIO CM.empty
    return $ MapState { sagaGraph = sagaVar
                      , charGraph = charVar
                      , schemaGraph = schemaVar
                      , resourceGraph = resVar
                      , schemaRawGraph = schemaRawVar
                      , resourceRawGraph = resRawVar
                      , characterMap = cm
                        } 


-- | Replace the raw character graph in the MapState.
-- All other elements are recalculated.
putCharGraph :: MapState -> G.RDFGraph
    -> STM.STM MapState 
putCharGraph st g = do
        res1 <- STM.readTVar $ resourceGraph st
        s1 <- STM.readTVar $ schemaGraph st
        let g1 = R.makeGraph  g s1 res1
        STM.writeTVar (charRawGraph st) g
        STM.writeTVar (charGraph st) g1
        let ll = C.characterFromGraph g1
        let clab = head ll
        let cid = getLocalID clab
        let cl = C.getAllCS g1 clab
        STM.writeTVar (characterID st)  $ fromJust cid
        STM.writeTVar (characterMap st) 
                $ CM.insertListS res1 CM.empty $ fromJust cl
        return $ st

-- | Return the state graph (i.e. character data) from STM.
getStateGraph :: MapState -> IO G.RDFGraph
getStateGraph st = STM.readTVarIO (charGraph st) 


-- | Return the schema from STM as an RDF Graph.
getSchemaGraph :: MapState -> IO G.RDFGraph
getSchemaGraph st = STM.readTVarIO ( schemaGraph st)

-- | Return the resource graph from STM as an RDF Graph.
getResourceGraph :: MapState -> IO G.RDFGraph
getResourceGraph st =  STM.readTVarIO (resourceGraph st)


-- | Return the sheet for a given character, season, and year (as RDFGraph).
lookup :: MapState          -- ^ Memory state
       -> String            -- ^ Character ID
       -> String            -- ^ Season
       -> Int               -- ^ Year
       -> IO (Maybe CM.CharacterRecord)
       -- ^ Character sheet as an RDF Graph
lookup st char season year = do
          let g = charGraph st
          let res = resourceGraph st
          print $ char ++ " - " ++ season ++ " - " ++ show year
          cmap <- STM.readTVarIO (characterMap st)
          print $  AR.armcharRes char
          let charstring = "armchar:" ++ char
          return $ CM.lookup cmap charstring season year


-- getResource :: G.RDFGraph -> G.RDFLabel -> Maybe G.RDFGraph
-- getResource g label = Nothing

-- | Update the state graph with the given Advancement object.
putAdvancement :: MapState -> TC.Advancement -> IO (Either G.RDFGraph String)
putAdvancement st adv = do 
         STM.atomically $ do
             g <- STM.readTVar (charRawGraph st)
             schema <- STM.readTVar $ schemaGraph st
             let advg = TC.makeRDFGraph adv
             let g1 = RP.persistGraph schema advg
             charg <- STM.readTVar (charGraph st)
             let g0 = RP.persistedGraph charg (TC.rdfid adv) 
             let gg = (g0 `G.delete` g) `G.addGraphs` g1
             newst <- putCharGraph st gg
             return $ Left gg
-- TODO: Check for conflicting advancements 

-- | Update character metadata.  This has not been tested and requirs
-- careful revision.
putCharacter :: MapState            -- ^ Memory state
             -> TC.Character        -- ^ New Character to be stored
             -> IO (Either G.RDFGraph String) 
                -- ^ Either the new character graph or an error message
putCharacter st char = do 
         STM.atomically $ do
             g <- STM.readTVar (charRawGraph st)
             schema <- STM.readTVar (schemaGraph st)

             chargraph <- STM.readTVar (charGraph st)
             let charg = TC.makeRDFGraph char
             let g1 = RP.persistChar schema charg
             let g0 = RP.persistedChar chargraph (TC.characterID char) 
             let gg = (g0 `G.delete` g) `G.addGraphs` g1

             newst <- putCharGraph st gg
             return $ Left gg

