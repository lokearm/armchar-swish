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
               , getState
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
import qualified ArM.Resources as AR
import           ArM.Rules.Aux
import qualified ArM.Rules.Persistence as RP
import qualified ArM.Rules as R
import           ArM.Resources


-- | The `MapState` object defines the state of the server.
-- The server process maintains a single `MapState` object in
-- software transactional memory (STM), recording all the data
-- which may potentially change during operation.
data MapState = MapState { charGraph :: STM.TVar G.RDFGraph
                         , schemaGraph :: G.RDFGraph
                         , resourceGraph :: G.RDFGraph
                         , charRawGraph :: STM.TVar G.RDFGraph
                         , schemaRawGraph :: G.RDFGraph
                         , resourceRawGraph :: G.RDFGraph
                         , characterLabel :: STM.TVar G.RDFLabel
                         , characterID :: STM.TVar String
                         , characterMap :: STM.TVar CM.CharacterMap
                           }

-- | Make the State object to be stored in STM.
-- The return value is Either a MapState object or an error message.
getState :: G.RDFGraph -> G.RDFGraph -> STM.STM (STM.TVar MapState)
getState res schema = do
    char <- STM.newTVar G.emptyGraph
    rawchar <- STM.newTVar G.emptyGraph
    cm <- STM.newTVar CM.empty
    cid <- STM.newTVar ""
    clab <- STM.newTVar (armRes "noSuchCharacter")
    STM.newTVar $ MapState
                      { charGraph = char
                      , schemaGraph = s1
                      , resourceGraph = res1
                      , charRawGraph = rawchar
                      , schemaRawGraph = schema
                      , resourceRawGraph = res
                      , characterLabel = clab
                      , characterID = cid
                      , characterMap = cm
                        }
   where s1 = R.prepareSchema schema
         res1 = R.prepareResources $ res `G.merge` s1 


-- | Replace the raw character graph in the MapState.
-- All other elements are recalculated.
putCharGraph :: (STM.TVar MapState) -> G.RDFGraph
    -> STM.STM (Either (STM.TVar MapState) String)
putCharGraph stateVar g = do
        st <- STM.readTVar stateVar
        let res1 = resourceGraph st
        let g1 = R.makeGraph  g (schemaGraph st) (resourceGraph st)
        STM.writeTVar (charRawGraph st) g
        STM.writeTVar (charGraph st) g1
        let ll = C.characterFromGraph g1
        let clab = head ll
        let cid = getLocalID clab
        let cl = C.getAllCS g1 clab
        STM.writeTVar (characterLabel st) clab
        STM.writeTVar (characterID st)  $ fromJust cid
        STM.writeTVar (characterMap st) 
                $ CM.insertListS res1 CM.empty $ fromJust cl
        return $ Left stateVar

-- | Return the state graph (i.e. character data) from STM.
getStateGraph :: STM.TVar MapState -> IO G.RDFGraph
getStateGraph st = STM.readTVarIO st
       >>= ( \ state -> STM.readTVarIO (charGraph state) )


-- | Return the schema from STM as an RDF Graph.
getSchemaGraph :: STM.TVar MapState -> IO G.RDFGraph
getSchemaGraph st = fmap schemaGraph $ STM.readTVarIO st

-- | Return the resource graph from STM as an RDF Graph.
getResourceGraph :: STM.TVar MapState -> IO G.RDFGraph
getResourceGraph st = fmap resourceGraph $ STM.readTVarIO st


-- | Return the sheet for a given character, season, and year (as RDFGraph).
lookup :: STM.TVar MapState -- ^ Memory state
       -> String            -- ^ Character ID
       -> String            -- ^ Season
       -> Int               -- ^ Year
       -> IO (Maybe CM.CharacterRecord)
       -- ^ Character sheet as an RDF Graph
lookup stateVar char season year = do
          st <- STM.readTVarIO stateVar
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
putAdvancement :: STM.TVar MapState -> TC.Advancement -> IO (Either G.RDFGraph String)
putAdvancement stateVar adv = do 
         STM.atomically $ do
             st <- STM.readTVar stateVar
             g <- STM.readTVar (charRawGraph st)
             let schema = schemaGraph st
             let advg = TC.makeRDFGraph adv
             let g1 = RP.persistGraph schema advg
             charg <- STM.readTVar (charGraph st)
             let g0 = RP.persistedGraph charg (TC.rdfid adv) 
             let gg = (g0 `G.delete` g) `G.addGraphs` g1
             newst <- putCharGraph stateVar gg
             case (newst) of
                Left s -> return $ Left gg
                Right x -> return $ Right x
-- TODO: Check for conflicting advancements 

-- | Update character metadata.  This has not been tested and requirs
-- careful revision.
putCharacter :: STM.TVar MapState   -- ^ Memory state
             -> TC.Character        -- ^ New Character to be stored
             -> IO (Either G.RDFGraph String) 
                -- ^ Either the new character graph or an error message
putCharacter stateVar char = do 
         STM.atomically $ do
             st <- STM.readTVar stateVar
             g <- STM.readTVar (charRawGraph st)
             let schema = schemaGraph st

             chargraph <- STM.readTVar (charGraph st)
             let charg = TC.makeRDFGraph char
             let g1 = RP.persistChar schema charg
             let g0 = RP.persistedChar chargraph (TC.characterID char) 
             let gg = (g0 `G.delete` g) `G.addGraphs` g1

             newst <- putCharGraph stateVar gg
             case (newst) of
                Left s -> return $ Left gg
                Right x -> return $ Right x

