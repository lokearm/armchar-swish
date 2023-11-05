{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.CharGraph
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.CharGraph 
               ( loadSaga
               , loadChar
               , MapState(..)
               ) where

import           ArM.Types.RDF (fromRDFGraph)

import qualified Swish.RDF.Graph as G

import qualified Data.Map as Map

import qualified ArM.Character.CharGen as TCG
import qualified ArM.Types.Saga as TS
import qualified ArM.Rules as R
import           ArM.IO

readAllFiles :: [String] -> IO [G.RDFGraph]
readAllFiles = mapM readGraph
mergeGraphs :: [G.RDFGraph] -> G.RDFGraph
mergeGraphs [] = G.emptyGraph
mergeGraphs (x:xs) = foldr G.merge x xs

data MapState = MapState 
              { saga :: TS.Saga
              , charList :: [G.RDFLabel]
              , schemaGraph :: G.RDFGraph
              , resourceGraph :: G.RDFGraph
              , schemaRawGraph :: [G.RDFGraph]
              , resourceRawGraph :: [G.RDFGraph]
              , cgMap :: Map.Map String TCG.CharGen
              }

loadSaga :: String -> IO MapState
loadSaga fn = do
    -- 1. Load Saga
    sgraph <- readGraph fn
    let sid = head $ TS.sagaFromGraph sgraph
    let sob = fromRDFGraph sgraph sid :: TS.Saga
    -- 2. Load Schema
    let schemaFN = TS.getSchemaFiles sid sgraph
    ss <- readAllFiles schemaFN
    -- 3. Load resources
    let resFN = TS.getResourceFiles sid sgraph
    rs <- readAllFiles resFN
    -- 4. Augment graphs
    let rawSchema = mergeGraphs ss
    let res = mergeGraphs rs
    let schema = R.prepareSchema rawSchema
    let res1 = R.prepareResources $ res `G.merge` schema 
    return MapState { saga = sob
                    , charList = []
                    , schemaGraph = schema
                    , resourceGraph = res1
                    , schemaRawGraph = ss
                    , resourceRawGraph = rs
                    , cgMap = Map.empty
                    }

loadChar :: MapState -> String -> IO TCG.CharGen
loadChar st fn = readGraph fn >>= ( return . TCG.makeCharGen schema res1 )
    where schema = schemaGraph st
          res1 = resourceGraph st

