module Main where

import System.IO as IO
import Data.Text.IO as DTIO
-- import Data.Text.Lazy.IO as DTLIO
import Control.Monad
import Swish.RDF.Formatter.Turtle
import qualified Data.Text.Lazy as  T
-- import Swish.Rule
import Swish.RDF.Graph
import Data.List


import ArM.Query
import ArM.Metadata
import ArM.Advancement
import Rules
import ArM.AuxIO
import ArM.Resources

import Swish.RDF.Ruleset



testCharacter = "armchar:cieran"

main :: IO ()
main = do
        character <- readGraph characterFile 
        schemaGraph <- readGraph armFile 
        resourceGraph <- readGraph resourceFile 
        let armGraph = merge schemaGraph resourceGraph
        let g = prepareInitialCharacter $ merge armGraph character 

        DTIO.putStrLn $ formatGraphAsText $ g
        let vb = getCharacterMetadata g testCharacter 
        print vb

        let z = sort $ getAdvancements g testCharacter 
        print z
