module Main where

import System.IO
import Data.Text.Lazy.IO as DTIO
import Control.Monad
import Swish.RDF.Parser.Turtle
import Swish.RDF.Formatter.Turtle
import Data.Text.Lazy




import Swish.RDF.Ruleset

armFile = "Ontology/arm.ttl"
resourceFile = "Ontology/resources.ttl"
characterFile = "Ontology/cieran.ttl"
baseURI = Nothing


main :: IO ()
main = do
        let list = []
        contents <- DTIO.readFile characterFile 
        let character = parseTurtle ( contents ) baseURI 
        case (character) of
           (Left s) -> print s
           (Right c) -> print $ formatGraphAsText c

