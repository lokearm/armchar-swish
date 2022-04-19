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
import Data.Maybe


import ArM.Query
import ArM.Metadata
import ArM.Advancement
import ArM.Rules
import ArM.AuxIO
import ArM.Resources
import ArM.Character

import Swish.RDF.Ruleset



testCharacter = "armchar:cieran"

-- | Construct the test graph
getGraph :: IO RDFGraph
getGraph = do
        character <- readGraph characterFile 
        schemaGraph <- readGraph armFile 
        resourceGraph <- readGraph resourceFile 
        let armGraph = merge schemaGraph resourceGraph
        return $ prepareInitialCharacter $ merge armGraph character 

main :: IO ()
main = do
        g <- getGraph


        DTIO.putStrLn $ formatGraphAsText $ g
        let vb = getCharacterMetadata g testCharacter 
        print vb

        let sheet0 = show $ fromJust $ getInitialSheet g testCharacter
        print sheet0

        let vb2 = getCharacterMetadata g sheet0
        print vb2

        -- let x = fwdApplySimple csRule g
        -- print x
        -- let y = fwdApplySimple csRule g0
        -- print y


        let x = getCharacter g testCharacter 
        print x

        -- let z = sort $ getPregameAdvancements g testCharacter 
        -- print z
