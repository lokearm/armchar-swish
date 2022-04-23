{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Load
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Functions to load the RDF graphs
--
-----------------------------------------------------------------------------

module ArM.Load where

import ArM.Resources
import ArM.Rules

import System.IO ( IO, readFile )
import Swish.RDF.Graph (emptyGraph,RDFGraph,merge)
import qualified Data.Text.Lazy.IO as DTLIO
import Swish.RDF.Parser.Turtle (parseTurtle)
import Control.Parallel

-- | Read an RDFGraph, ignoring errors.
readGraph :: String -> IO RDFGraph
readGraph fn = do
        contents <- DTLIO.readFile fn 
        case ( parseTurtle ( contents ) baseURI ) of
           (Left s ) -> do
               print s
               return emptyGraph
           (Right g ) -> return g

-- | Load the different graph and make initial inferences
-- See diagram in README.
getGraph :: String -> String -> String -> IO (RDFGraph,RDFGraph,RDFGraph)
getGraph characterFile armFile resourceFile = do
        character <- readGraph characterFile 
        let characterGraph' = prepareCS character
        schemaGraph' <- readGraph armFile 
        let schemaGraph = prepareSchema schemaGraph'
        resourceGraph' <- readGraph resourceFile 
        let resourceGraph = prepareResources resourceGraph'
        let characterGraph =
              prepareInitialCharacter $ merge schemaGraph characterGraph'
        let charGraph = prepareGraph $ merge resourceGraph characterGraph 
        return $ schemaGraph `par` resourceGraph `par`
               characterGraph `pseq`
               ( charGraph, schemaGraph, resourceGraph )

