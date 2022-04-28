module ArM.STM where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import Swish.RDF.Graph (RDFGraph)
import Data.Maybe (fromJust)

import qualified ArM.CharacterMap as CM
import qualified ArM.Character as C
import qualified ArM.Resources as AR



data MapState = MapState { graph :: RDFGraph, resourceGraph :: RDFGraph }

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

update :: STM.TVar MapState -> RDFGraph-> IO (Maybe URI)
update g = Nothung

post :: STM.TVar MapState -> RDFGraph-> IO (Maybe URI)
post g = Nothung
