module Main where

import System.IO as IO
import Data.Text.IO as DTIO
-- import Data.Text.Lazy.IO as DTLIO
import Control.Monad
import Swish.RDF.Formatter.Turtle
import qualified Data.Text.Lazy as  T
-- import Swish.Rule
import Swish.RDF.Graph


import ArM.Metadata
import Rules
import ArM.AuxIO
import Resources

import Swish.RDF.Ruleset



testCharacter = "<https://hg.schaathun.net/armchar/character/cieran>"
-- testCharacter = "armchar:cieran"

main :: IO ()
main = do
        let list = []
        character <- readGraph characterFile 
        schemaGraph <- readGraph armFile 
        resourceGraph <- readGraph resourceFile 
        let armGraph = merge schemaGraph resourceGraph
        let m  = prepareInitialCharacter character
        let g = merge armGraph m 
        DTIO.putStrLn $ formatGraphAsText $ g
        let vb = getCharacterMetadata g testCharacter 
        print vb

