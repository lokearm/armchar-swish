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
-----------------------------------------------------------------------------
module ArM.STM where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe (fromJust)
import Network.URI (URI)
import Swish.QName (getLName)
import Data.Text (unpack)

import qualified ArM.CharacterMap as CM
import qualified ArM.Character as C
import qualified ArM.Types.Character as TC
import qualified ArM.Resources as AR

import ArM.Character.Metadata (characterFromGraph)
import ArM.Rules.Aux
import ArM.Rules (makeGraphs)
import ArM.Resources
import Swish.RDF.Graph

import Data.Set (fromList)
import Swish.Namespace (ScopedName,getScopeLocal)


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
                           characterLabel :: RDFLabel,
                           characterID :: String,
                           characterMap :: CM.CharacterMap
                           }

-- getSTM res schema g = STM.newTVarIO $ fromJust $ getState res schema g 
-- | Make the State object to be stored in STM.
-- The return value is Either a MapState object or an error message.
getState :: RDFGraph -> RDFGraph -> RDFGraph -> Either MapState String
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
         ll = characterFromGraph g1
         clab = head ll
         cid = getLocalID clab
         cl = C.getAllCS g1 clab
-- | Extract the local name (as String) from an RDFLabel.
getLocalID :: RDFLabel -> Maybe String
getLocalID lab = f $ fromRDFLabel lab 
      where f Nothing = Nothing
            f (Just x) = Just $ unpack $ getLName $ getScopeLocal x

-- | Return the state graph (i.e. character data) from STM.
getStateGraph :: STM.TVar MapState -> IO G.RDFGraph
getStateGraph st = fmap charGraph $ STM.readTVarIO st
-- | Return the schema from STM as an RDF Graph.
getSchemaGraph :: STM.TVar MapState -> IO G.RDFGraph
getSchemaGraph st = fmap schemaGraph $ STM.readTVarIO st
-- | Return the resource graph from STM as an RDF Graph.
getResourceGraph :: STM.TVar MapState -> IO G.RDFGraph
getResourceGraph st = fmap resourceGraph $ STM.readTVarIO st

persistGraph g = foldGraphs $ Q.rdfQuerySubs vb tg
    where vb = Q.rdfQueryFind qg g
          qg = G.toRDFGraph $ fromList [ arc sVar pVar cVar,
                       arc pVar typeRes armPersistentProperty ]
          tg = G.toRDFGraph $ fromList [ arc sVar pVar cVar ]

persistRule = makeCRule "persistRule" 
    [ arc sVar pVar cVar,
      arc pVar typeRes armPersistentProperty ]
    [ arc sVar pVar cVar ]
persistGraph' g = fwdApplySimple persistRule g

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


putGraph :: G.RDFGraph -> G.RDFGraph -> G.RDFGraph -> G.RDFGraph
putGraph g g0 g1 = G.merge (G.delete g0 g) g1

-- | Update the state graph with the given Advancement object.
putAdvancement :: STM.TVar MapState -> TC.Advancement -> IO G.RDFGraph
putAdvancement stateVar adv = do 
         STM.atomically $ do
             st <- STM.readTVar stateVar
             let g = charGraph st
             let schema = schemaGraph st
             let g1 = persistGraph $ merge schema $ TC.makeRDFGraph adv
             let adv0 = TC.fromRDFGraph g (TC.rdfid adv) :: TC.Advancement
             let g0 = TC.makeRDFGraph adv0
             let gg = putGraph g g0 g1
             STM.writeTVar stateVar $ st { charGraph = gg }
             return g1
          
-- TODO: Check for conflicting advancements 

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
