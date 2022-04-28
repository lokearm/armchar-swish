module ArM.STM where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Swish.RDF.Graph as G
import Data.Maybe (fromJust)
import Network.URI (URI)

import qualified ArM.CharacterMap as CM
import qualified ArM.Character as C
import qualified ArM.Resources as AR



data MapState = MapState { graph :: G.RDFGraph, resourceGraph :: G.RDFGraph }

getSTM res g char = STM.newTVarIO MapState { graph = g, resourceGraph = res  }

lookup :: STM.TVar MapState -> String -> String -> Int 
       -> IO (Maybe CM.CharacterRecord)
lookup stateVar char season year = do
          st <- STM.readTVarIO stateVar
          let g = graph st
          let res = resourceGraph st
          print $ char ++ " - " ++ season ++ " - " ++ show year
          let cl =  C.getAllCS g $ AR.armcharRes char
          print $  AR.armcharRes char
          let charstring = "armchar:" ++ char
          case (cl) of
             Nothing -> return Nothing
             (Just x) -> return $ CM.lookup cmap charstring season year
                where  cmap = CM.insertListS res CM.empty $ x

getResource :: G.RDFGraph -> G.RDFLabel -> Maybe G.RDFGraph
getResource g label = Nothing

updateResource :: G.RDFGraph -> G.RDFLabel -> G.RDFGraph -> Maybe G.RDFGraph
updateResource g label g1 
          | Nothing == oldres = Nothing
          | otherwise  = Just $ G.merge ( G.delete g $ fromJust oldres ) g1
          where oldres =  getResource g label

putGraph :: G.RDFGraph -> G.RDFLabel -> G.RDFGraph -> Maybe G.RDFGraph
putGraph g label g1 
          | Nothing == oldres = Just $ G.merge g g1
          | otherwise  = Nothing
          where oldres =  getResource g label

putResource :: STM.TVar MapState -> G.RDFLabel -> G.RDFGraph
             -> IO (Maybe G.RDFGraph)
putResource stateVar label newres =  do
      STM.atomically $ do
          st <- STM.readTVar stateVar
          let g = graph st
          case (updateResource g label newres) of
             Nothing -> return Nothing
             (Just gg) -> do
                          STM.writeTVar stateVar $ st { graph = gg }
                          return $ Just gg


postResource :: STM.TVar MapState -> G.RDFLabel -> G.RDFGraph
             -> IO (Maybe G.RDFGraph)
postResource stateVar label newres = do 
      STM.atomically $ do
          st <- STM.readTVar stateVar
          let g = graph st
          case (putGraph g label newres) of
             Nothing -> return Nothing
             (Just gg) -> do
                          STM.writeTVar stateVar $ st { graph = gg }
                          return $ Just gg
