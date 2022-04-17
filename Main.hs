module Main where

import System.IO
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
        handle <- openFile characterFile ReadMode
        contents <- hGetContents handle
        let character = parseTurtle ( pack contents ) baseURI 
        case (character) of
           (Left s) -> print s
           (Right c) -> print $ formatGraphAsText c
        hClose handle

