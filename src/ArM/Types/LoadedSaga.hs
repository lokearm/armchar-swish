
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.LoadedSaga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Types.LoadedSaga
               ( LoadedSaga(..)
               , loadSaga
               , loadChars
               , sagaGraph
               ) where

import qualified Swish.RDF.Graph as G

import qualified Data.Map as Map

import qualified ArM.Types.Saga as TS
import qualified ArM.Rules as R
import           ArM.IO


readAllFiles :: [String] -> IO [G.RDFGraph]
readAllFiles = mapM readGraph
mergeGraphs :: [G.RDFGraph] -> G.RDFGraph
mergeGraphs [] = G.emptyGraph
mergeGraphs (x:xs) = foldr G.merge x xs

data LoadedSaga = LoadedSaga 
              { saga :: TS.Saga
              , schemaGraph :: G.RDFGraph
              , resourceGraph :: G.RDFGraph
              , schemaRawGraph :: [G.RDFGraph]
              , resourceRawGraph :: [G.RDFGraph]
              , cgMap :: Map.Map String G.RDFGraph
              }

-- ^ Load a saga from an RDF file and instantiate a LoadedSaga
-- The file should define both a covenant resoruce and a saga resource
loadSaga :: String -> IO LoadedSaga
loadSaga fn = do
    -- 1. Load Saga
    sgraph <- readGraph fn
    let sob = TS.sagaFromRDF sgraph 
    -- 2. Load Schema
    ss <- readAllFiles $ TS.schemaFiles sob
    -- 3. Load resources
    rs <- readAllFiles $ TS.resourceFiles sob
    -- 4. Augment graphs
    let rawSchema = mergeGraphs ss
    let res = mergeGraphs rs
    let schema = R.prepareSchema rawSchema
    let res1 = R.prepareResources $ res `G.merge` schema 
    return LoadedSaga { saga = sob
                    , schemaGraph = schema
                    , resourceGraph = res1
                    , schemaRawGraph = ss
                    , resourceRawGraph = rs
                    , cgMap = Map.empty
                    }

sagaGraph :: LoadedSaga -> G.RDFGraph 
sagaGraph = TS.sagaGraph . saga

-- | Load all associated characters into a LoadedSaga object
loadChars :: LoadedSaga -> IO LoadedSaga
loadChars s = mapM readGraph fs
      >>= ( \ cs -> return $ s { cgMap = foldl ins m $ zip fs cs } )
      where m = cgMap s 
            fs = TS.characterFiles $ saga s
            ins c (x,y) = Map.insert x y c

