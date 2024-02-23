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
module ArM.Types.MapState
               ( loadSaga
               , loadChar
               , loadChars
               , MapState(..)
               ) where

import qualified Swish.RDF.Graph as G

import qualified Data.Map as Map

import qualified ArM.Types.CharGen as TCG
import qualified ArM.Types.SagaFile as TS
import qualified ArM.Rules as R
import           ArM.IO


readAllFiles :: [String] -> IO [G.RDFGraph]
readAllFiles = mapM readGraph
mergeGraphs :: [G.RDFGraph] -> G.RDFGraph
mergeGraphs [] = G.emptyGraph
mergeGraphs (x:xs) = foldr G.merge x xs

data MapState = MapState 
              { saga :: TS.SagaFile
              , covenant :: TCG.CharGen
              , charList :: [G.RDFLabel]
              , schemaGraph :: G.RDFGraph
              , resourceGraph :: G.RDFGraph
              , schemaRawGraph :: [G.RDFGraph]
              , resourceRawGraph :: [G.RDFGraph]
              , cgMap :: Map.Map String TCG.CharGen
              }

-- ^ Load a saga from an RDF file and instantiate a MapState
-- The file should define both a covenant resoruce and a saga resource
loadSaga :: String -> IO MapState
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
    return MapState { saga = sob
                    , covenant = TCG.makeCharGen schema res1 sgraph
                    , charList = []
                    , schemaGraph = schema
                    , resourceGraph = res1
                    , schemaRawGraph = ss
                    , resourceRawGraph = rs
                    , cgMap = Map.empty
                    }

-- | Load a single character from file.
loadChar :: MapState  -- ^ MapState defining schema and resources
           -> String  -- ^ filename
           -> IO TCG.CharGen
loadChar st fn = do
    g <- readGraph fn 
    return $ (TCG.makeCharGen schema res1 g) { TCG.charFile = fn }  
    where schema = schemaGraph st
          res1 = resourceGraph st

-- | Get the character files associate with the saga
getCharFiles :: MapState -> [String]
getCharFiles = TS.characterFiles . saga

-- | Load all associated characters into a MapState object
loadChars :: MapState -> IO MapState
loadChars st = do
      cs <- mapM (loadChar st) fs
      let cns = map TCG.charID cs
      let ss = map show cns
      let ps = zip ss cs
      return $ st { charList = cns, cgMap = foldl ins m ps }
      where m = cgMap st 
            fs = getCharFiles st
            ins c (x,y) = Map.insert x y c

