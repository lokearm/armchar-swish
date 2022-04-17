module Main where

import System.IO as IO
import Data.Text.IO as DTIO
import Data.Text.Lazy.IO as DTLIO
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
        contents <- DTLIO.readFile characterFile 
        let character = parseTurtle ( contents ) baseURI 
        case (character) of
           (Left s) -> IO.putStrLn s
           (Right c) -> DTIO.putStrLn $ formatGraphAsText c

