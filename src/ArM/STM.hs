module ArM.STM where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe (fromJust)
import Network.URI (URI)

import qualified ArM.CharacterMap as CM
import qualified ArM.Character as C
import qualified ArM.Types.Character as TC
import qualified ArM.Resources as AR

import ArM.Rules.Aux
import ArM.Rules (makeGraphs)
import ArM.Resources
import Swish.RDF.Graph

import Data.Set (fromList)



data MapState = MapState { charGraph :: G.RDFGraph,
                           schemaGraph :: G.RDFGraph,
                           resourceGraph :: G.RDFGraph,
                           charRawGraph :: G.RDFGraph,
                           schemaRawGraph :: G.RDFGraph,
                           resourceRawGraph :: G.RDFGraph }

getSTM res schema g = STM.newTVarIO MapState { charGraph = g1,
                                               schemaGraph = s1,
                                               resourceGraph = res1,
                                               charRawGraph = g,
                                               schemaRawGraph = schema,
                                               resourceRawGraph = res
                                               }
   where (g1,s1,res1) = makeGraphs (g,schema,res)

rawTriple st = (charRawGraph st, schemaRawGraph st, resourceRawGraph st)
graphTriple st = (charGraph st, schemaGraph st, resourceGraph st)

t1 (a,b,c) = a
t2 (a,b,c) = b
t3 (a,b,c) = c

-- getGraph st = t1 . graphTriple

getStateGraph :: STM.TVar MapState -> IO G.RDFGraph
getStateGraph st = fmap charGraph $ STM.readTVarIO st
getSchemaGraph :: STM.TVar MapState -> IO G.RDFGraph
getSchemaGraph st = fmap schemaGraph $ STM.readTVarIO st
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

lookup :: STM.TVar MapState -> String -> String -> Int 
       -> IO (Maybe CM.CharacterRecord)
lookup stateVar char season year = do
          st <- STM.readTVarIO stateVar
          let g = charGraph st
          let res = resourceGraph st
          print $ char ++ " - " ++ season ++ " - " ++ show year
          let cl =  C.getAllCS g $ AR.armcharRes char
          print $  AR.armcharRes char
          let charstring = "armchar:" ++ char
          case (cl) of
             Nothing -> return Nothing
             (Just x) -> return $ CM.lookup cmap charstring season year
                where  cmap = CM.insertListS res CM.empty $ x


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
