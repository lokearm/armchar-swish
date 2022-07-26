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
import ArM.Rules.RDFS

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
-- Note: the third graph `res` has merged the schema and the resource
-- graph.  This is not reflected in the diagram.
getGraph :: String -> String -> String -> IO (RDFGraph,RDFGraph,RDFGraph)
getGraph f1 f2 f3 = fmap makeGraph $ getRawGraph f1 f2 f3
getRawGraph :: String -> String -> String -> IO (RDFGraph,RDFGraph,RDFGraph)
getRawGraph characterFile armFile resourceFile = do
        c0 <- readGraph characterFile 
        s0 <- readGraph armFile 
        res0 <- readGraph resourceFile 
        return (c0,s0,res0)

makeGraph (c0,s0,res0) = s1 `par` res1 `par` res2 `par` c2 `pseq` ( c3, s1, res2 )
    where c1 = prepareCharGraph c0
          s1 = prepareSchema s0
          res1 = prepareResources res0
          c2 = prepareInitialCharacter $ merge s1 c1
          c3 = prepareGraph $ merge res1 c2 
          res2 = applyRDFS $ merge s1 res1

