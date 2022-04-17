module AuxIO where

import System.IO as IO
import Swish.RDF.Graph
import Data.Text.Lazy.IO as DTLIO
import Swish.RDF.Parser.Turtle
import Resources

readGraph :: String -> IO RDFGraph
readGraph fn = do
        contents <- DTLIO.readFile fn 
        case ( parseTurtle ( contents ) baseURI ) of
           (Left _ ) -> return emptyGraph
           (Right g ) -> return g
