{-# LANGUAGE OverloadedStrings #-}
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
-- using three RDF graphs which can be read from file (`ArM.IO` module).
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
               , ArM.STM.lookupIO
               , ArM.STM.lookupChar
               , ArM.STM.lookupCharIO
               , loadSaga
               , getStateGraph
               , getSaga
               , getSchemaGraph
               , getResourceGraph
               , cleanAdvancement
               , putAdvancement
               , putCharacter
               , putCharGraph 
               , MapState(..)
               , getCast
               ) where

import Prelude hiding (lookup)
import Swish.RDF.Formatter.Turtle (formatGraphIndent)
import qualified GHC.Conc as STM
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM.Map as M
import qualified Swish.RDF.Graph as G
import           Data.Maybe (fromJust)

-- import qualified ArM.Character as C
import           ArM.Types.RDF (makeRDFGraph,fromRDFGraph)
import qualified ArM.Types.Season as TS
import qualified ArM.Types.Character as TC
import qualified ArM.Types.Advancement as TA
import qualified ArM.Character.CharGen as TCG
import qualified ArM.Types.Saga as TS
-- import qualified ArM.Resources as AR
-- import           ArM.Rules.Aux
import qualified ArM.Rules.Persistence as RP
import qualified ArM.Rules as R
import           ArM.Resources
import           ArM.IO

import ArM.Trace

-- | The `MapState` object defines the state of the server.
-- The server process maintains a single `MapState` object in
-- software transactional memory (STM), recording all the data
-- which may potentially change during operation.
data MapState = MapState 
              { saga :: STM.TVar TS.Saga
              , charList :: STM.TVar [G.RDFLabel]
              , schemaGraph :: STM.TVar G.RDFGraph
              , resourceGraph :: STM.TVar G.RDFGraph
              , schemaRawGraph :: STM.TVar [G.RDFGraph]
              , resourceRawGraph :: STM.TVar [G.RDFGraph]
              , cgMap :: M.Map String TCG.CharGen
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
    sgraph <- readGraph fn
    let sid = head $ TS.sagaFromGraph sgraph
    let sob = fromRDFGraph sgraph sid :: TS.Saga
    sagaVar <- STM.newTVarIO sob
    -- 2. Load Schema
    let schemaFN = TS.getSchemaFiles sid sgraph
    ss <- readAllFiles schemaFN
    schemaRawVar <- STM.newTVarIO ss
    -- 3. Load resources
    let resFN = TS.getResourceFiles sid sgraph
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
    let charFN = TS.getCharacterFiles sid sgraph
    cs <- readAllFiles charFN

    clVar <- STM.newTVarIO []

    cgm <- STM.atomically  M.empty
    let st = MapState { saga = sagaVar
                      , charList = clVar
                      , schemaGraph = schemaVar
                      , resourceGraph = resVar
                      , schemaRawGraph = schemaRawVar
                      , resourceRawGraph = resRawVar
                      , cgMap = cgm
                      } 
    putStrLn "Ready to put graphs"
    mapM_ (putCharGraph st) cs
    putStrLn "graphs put"
    return st

-- | Replace the raw character graph in the MapState.
-- All other elements are recalculated.
putCharGraph :: MapState -> G.RDFGraph -> IO MapState 
putCharGraph st g = trace "putCharGraph" $ do
        res1 <- STM.readTVarIO $ resourceGraph st
        schema1 <- STM.readTVarIO $ schemaGraph st
        let cgen = TCG.makeCharGen schema1 res1 g
        let clab = TCG.charID cgen
        let cmap = cgMap st
        let clVar = charList st
        putStrLn "Ready to insert CharGen object"
        STM.atomically $ do
           cl1 <- STM.readTVar clVar 
           STM.writeTVar clVar (clab:cl1)
           M.insert (show clab) cgen cmap
        print $ "putCharGraph " ++ show clab
        let cls = TCG.charSheets cgen
        print $ "Number of character stages: " ++ show (length cls)
        print cgen
        return $ st


-- | Return the state graph (i.e. character data) from STM.
getStateGraph :: MapState -> IO [G.RDFLabel]
getStateGraph st = STM.readTVarIO (charList st) 


-- | Return the schema from STM as an RDF Graph.
getSaga :: MapState -> IO TS.Saga
getSaga st = STM.readTVarIO ( saga st)

-- | Return the schema from STM as an RDF Graph.
getSchemaGraph :: MapState -> IO G.RDFGraph
getSchemaGraph st = STM.readTVarIO ( schemaGraph st)

-- | Return the resource graph from STM as an RDF Graph.
getResourceGraph :: MapState -> IO G.RDFGraph
getResourceGraph st =  STM.readTVarIO (resourceGraph st)

-- | Return the sheet for a given character, season, and year (as RDFGraph).
lookupCharIO :: MapState          -- ^ Memory state
       -> String            -- ^ Character ID
       -> IO (Maybe TCG.CharGen)
lookupCharIO m = STM.atomically . lookupChar m
lookupChar :: MapState          -- ^ Memory state
       -> String            -- ^ Character ID
       -> STM.STM (Maybe TCG.CharGen)
lookupChar st char = M.lookup (strace $ show (armcharRes char)) cmap 
         where cmap = cgMap st


-- | Return the sheet for a given character, season, and year (as RDFGraph).
lookupIO :: MapState          -- ^ Memory state
       -> String            -- ^ Character ID
       -> TS.CharTime       -- ^ Season/Year or Development Stage
       -> IO (Maybe G.RDFGraph)
lookupIO m c t  = STM.atomically $ lookup m c t
lookup :: MapState          -- ^ Memory state
       -> String            -- ^ Character ID
       -> TS.CharTime       -- ^ Season/Year or Development Stage
       -> STM.STM (Maybe G.RDFGraph)
lookup st char t = do
          let cmap = cgMap st
          let k = strace $ show (armcharRes char)
          cg <- M.lookup k cmap 
          case (cg) of
             Nothing -> return $ trace "lookup: character not found" Nothing
             Just cg1 -> do
                let csl = TCG.charSheets cg1
                case ( trace (show $ map TS.timeOf csl) $ TCG.findSeason csl t ) of
                   Nothing -> return $ trace "lookup: season not found" Nothing
                   Just cstage -> return $ Just $ TCG.sheetGraph cstage

-- getResource :: G.RDFGraph -> G.RDFLabel -> Maybe G.RDFGraph
-- getResource g label = Nothing

-- | Clean up a submitted Advancement object.
-- This should always be done before `putAdvancement` is called.
-- The reason not to embed this into the `putAdvancement` function itself
-- is to make a separate error message on failure to allow `WebService` to
-- report distinct errors differently.
cleanAdvancement :: MapState -> TA.Advancement 
               -> IO (Either TA.Advancement String) 
                  -- ^ Either a cleaned up Advancement or an error message
cleanAdvancement st adv = do 
         -- Reformat adv into adv1
         -- This removes non-editable properties form the input and
         -- regenerates calculated fields.
         let advg = makeRDFGraph adv
         let clab = TA.advChar adv
         putStrLn $ "STM.cleanAdvancement: " ++ show clab
         schema <- STM.readTVarIO $ schemaGraph st
         res1 <- STM.readTVarIO $ resourceGraph st
         let advg1 = RP.persistGraph schema advg
         liftIO $ putStrLn "advg1"
         liftIO $ print $ formatGraphIndent "\n" True advg1
         liftIO $ putStrLn "== advg1"
         let newg = R.makeGraph advg1 schema res1
         let as = TA.getAllAdvancements newg clab
         liftIO $ print $ "clab " ++ show clab
         liftIO $ print $ "rdfid " ++ show (TA.rdfid adv)
         case as of
            [] -> return $ Right "No advancements in input"
            (x:[]) -> return $ Left x
            (_:_:_) -> return $ Right "Multiple advancements not supported."

-- | Update the state graph with the given Advancement object.
putAdvancement :: MapState -> TA.Advancement -> IO (Either String String)
putAdvancement st adv = 
         let cgm = cgMap st 
             clab = TA.advChar adv in
         STM.atomically $ do
             cgen <- M.lookup (show clab) cgm

             case (cgen) of
                Nothing -> return $ Right $ "No such character: " ++ show clab
                Just cgen0 -> do
                   schema <- STM.readTVar $ schemaGraph st
                   res1 <- STM.readTVar $ resourceGraph st
                   let cgen1 = TCG.putAdvancement schema res1 cgen0 adv
                   M.insert (show clab) cgen1 cgm
                   return $ Left "Advancement Inserted"

-- | Update character metadata.  This has not been tested and requirs
-- careful revision.
putCharacter :: MapState            -- ^ Memory state
             -> TC.Character        -- ^ New Character to be stored
             -> IO (Either String String) 
                -- ^ Either the new character graph or an error message
putCharacter st char = 
         let cgm = cgMap st 
             clab = TC.characterID char in
         STM.atomically $ do
             cgen <- M.lookup (show clab) cgm
             case (cgen) of
                Nothing -> return $ Right $ "No such character: " ++ show clab
                Just cgen0 -> do
                    chgraph <- cleanCharacter st char
                    schema <- STM.readTVar $ schemaGraph st
                    res1 <- STM.readTVar $ resourceGraph st
                    let cgen1 = TCG.putCharacter schema res1 cgen0 chgraph
                    M.insert (show clab) cgen1 cgm
                    return $ Left "Metadata inserted."
                    -- TODO Regenerate CharGen object

-- | Auxiliary to `putCharacter`.
-- Clean up the input, converting it to a graph.
-- There is no actual error checking.
cleanCharacter :: MapState     -- ^ The memory state
               -> TC.Character -- ^ User input to be cleaned
               -> STM.STM G.RDFGraph 
                  -- ^ Cleaned up graph representation of the Character
cleanCharacter st ch = do
         -- Removes non-editable properties form the input and
         -- regenerates calculated fields.
         let chgraph = makeRDFGraph ch
         schema <- STM.readTVar $ schemaGraph st 
         return $ RP.persistChar schema chgraph

getCharacter :: MapState -> G.RDFLabel -> IO TC.Character
getCharacter st label = fmap fromJust (lookupCharIO st (show label))
                      >>= return . TC.getCharacter . TCG.charGraph 

getCast :: MapState -> IO [TC.Character]
getCast st = STM.readTVarIO (charList st) >>= mapM (getCharacter st)
