module ArM.STM where

import qualified ArM.CharacterMap as CM
import qualified Control.Concurrent.STM as STM
import qualified ArM.Character as C
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)


data MapState = MapState { stMap :: CM.CharacterMap }

getSTM res g char = STM.newTVarIO MapState { stMap = cmap }
     where cl =  fromJust $ C.getAllCS g char
           cmap = CM.insertListS res CM.empty $ cl

lookup :: STM.TVar MapState -> String -> String -> Int -> IO (Maybe CM.CharacterRecord)
lookup stateVar char season year = do
          cmap <- stMap <$> STM.readTVarIO stateVar
          return $ CM.lookup cmap char season year
