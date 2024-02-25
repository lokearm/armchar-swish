{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Types.Saga
               ( Saga(..)
               , parseSaga
               , loadChar
               -- , loadChars
               , schemaGraph
               , resourceGraph
               ) where

import qualified Swish.RDF.Graph as G

import qualified Data.Map as Map

import qualified ArM.Types.CharGen as TCG
import qualified ArM.Types.LoadedSaga as LS
import Data.Maybe (fromJust)


data Saga = Saga 
              { saga :: LS.LoadedSaga
              , covenant :: TCG.CharGen
              , charList :: [G.RDFLabel]
              , cgMap :: Map.Map String TCG.CharGen
              }

schemaGraph :: Saga-> G.RDFGraph
schemaGraph = LS.schemaGraph . saga
resourceGraph :: Saga-> G.RDFGraph
resourceGraph = LS.resourceGraph . saga
{-
schemaRawGraph :: Saga-> [G.RDFGraph]
schemaRawGraph = LS.schemaRawGraph . saga
resourceRawGraph :: Saga-> [G.RDFGraph]
resourceRawGraph = LS.resourceRawGraph . saga
-}

-- ^ Load a saga from an RDF file and instantiate a Saga
-- The file should define both a covenant resoruce and a saga resource
parseSaga :: LS.LoadedSaga -> Saga
parseSaga sob = 
    Saga { saga = sob
         , covenant = TCG.makeCharGen schema res1 g
         , charList = cns
         , cgMap = foldl ins Map.empty ps 
         }
    where 
          schema = LS.schemaGraph sob
          res1 = LS.resourceGraph sob
          g = LS.sagaGraph sob
          ins c (x,y) = Map.insert x y c
          cs = map (loadChar sob) $ Map.keys $ LS.cgMap sob
          cns = map TCG.charID cs
          ps = zip (map show cns) cs

-- | Load a single character from file.
loadChar :: LS.LoadedSaga  -- ^ Saga object with schema and resources
           -> String  -- ^ filename
           -> TCG.CharGen
loadChar st fn = (TCG.makeCharGen schema res1 g) { TCG.charFile = fn }  
    where schema = LS.schemaGraph st
          res1 = LS.resourceGraph st
          g = fromJust $ Map.lookup fn (LS.cgMap  st) 

