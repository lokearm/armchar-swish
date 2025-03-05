{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.IO
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Functions to load the RDF graphs
--
-----------------------------------------------------------------------------

module ArM.Swish.IO (readGraph) where

import           ArM.Swish.Resources (baseURI)
import           Swish.RDF.Graph (emptyGraph,RDFGraph)
import qualified Data.Text.Lazy.IO as DTLIO
import           Swish.RDF.Parser.Turtle (parseTurtle)

-- | Read an RDFGraph, ignoring errors.
readGraph :: String -> IO RDFGraph
readGraph fn = do
        contents <- DTLIO.readFile fn 
        case ( parseTurtle ( contents ) baseURI ) of
           (Left s ) -> do
               print $ "Error reading " ++ fn
               print s
               return emptyGraph
           (Right g ) -> return g
