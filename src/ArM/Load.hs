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

module ArM.Load (getRawGraph) where

import           ArM.Resources (baseURI)
import           System.IO ( IO )
import           Swish.RDF.Graph (emptyGraph,RDFGraph)
import qualified Data.Text.Lazy.IO as DTLIO
import           Swish.RDF.Parser.Turtle (parseTurtle)

-- | Read an RDFGraph, ignoring errors.
readGraph :: String -> IO RDFGraph
readGraph fn = do
        contents <- DTLIO.readFile fn 
        case ( parseTurtle ( contents ) baseURI ) of
           (Left s ) -> do
               print s
               return emptyGraph
           (Right g ) -> return g

-- | Load the different graph.
getRawGraph :: String -- ^ file name for the character data
            -> String -- ^ file name for the schema graph
            -> String -- ^ file name for the resource graph
            -> IO (RDFGraph,RDFGraph,RDFGraph)
            -- ^ The character, schema, and resource graphs
getRawGraph characterFile armFile resourceFile = do
        c0 <- readGraph characterFile 
        s0 <- readGraph armFile 
        res0 <- readGraph resourceFile 
        return (c0,s0,res0)


