module Main where

import System.IO as IO
import Data.Text.IO as DTIO
import Data.Text.Lazy.IO as DTLIO
import Control.Monad
import Swish.RDF.Parser.Turtle
import Swish.RDF.Formatter.Turtle
import qualified Data.Text.Lazy as  T
import Swish.Rule
import Swish.RDF.Graph


import Rules

import Swish.RDF.Ruleset

armFile = "Ontology/arm.ttl"
resourceFile = "Ontology/resources.ttl"
characterFile = "Ontology/cieran.ttl"
baseURI = Nothing

fwdApplySimple :: RDFRule -> (Either String RDFGraph) -> [RDFGraph]
fwdApplySimple _ (Left _) = [emptyRDFGraph]
fwdApplySimple r (Right c) = fwdApply r [c]


main :: IO ()
main = do
        let list = []
        contents <- DTLIO.readFile characterFile 
        let character = parseTurtle ( contents ) baseURI 
        case (character) of
           (Left s) -> IO.putStrLn s
           (Right c) -> do
              DTIO.putStrLn $ formatGraphAsText c
        let cs  = fwdApplySimple csRule character
        print cs
        DTIO.putStrLn $ formatGraphAsText $ head cs

