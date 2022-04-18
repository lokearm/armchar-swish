-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.AuxIO
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- IO specific for ArM ontologies and characters
--
-----------------------------------------------------------------------------

module ArM.AuxIO where

import System.IO as IO
import Swish.RDF.Graph
import Data.Text.Lazy.IO as DTLIO
import Swish.RDF.Parser.Turtle
import ArM.Resources

-- | Read an RDFGraph, ignoring errors.
readGraph :: String -> IO RDFGraph
readGraph fn = do
        contents <- DTLIO.readFile fn 
        case ( parseTurtle ( contents ) baseURI ) of
           (Left s ) -> do
               print s
               return emptyGraph
           (Right g ) -> return g
