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
getGraph :: String -> String -> String -> IO RDFGraph
getGraph characterFile armFile resourceFile = do
        character <- readGraph characterFile 
        schemaGraph' <- readGraph armFile 
        resourceGraph' <- readGraph resourceFile 
        let resourceGraph = prepareResources resourceGraph'
        let schemaGraph = prepareSchema schemaGraph'
        let characterGraph' = prepareCS character
        let characterGraph =
              prepareInitialCharacter $ merge schemaGraph characterGraph'
        return $ prepareGraph $ merge resourceGraph characterGraph 

