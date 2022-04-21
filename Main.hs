{-# LANGUAGE OverloadedStrings #-}
-- (C) 2022: Hans Georg Schaathun <hg+gamer@schaathun.net>

module Main where

import System.IO as IO
import qualified Data.Text.IO as DTIO
-- import Data.Text.Lazy.IO as DTLIO
import Control.Monad
import Swish.RDF.Formatter.Turtle
import qualified Data.Text.Lazy as  T
-- import Swish.Rule
import Swish.RDF.Graph
import Data.List
import Data.Maybe
import Data.Text (Text)


import ArM.Query
import ArM.Metadata
import ArM.Advancement
import ArM.Load
import ArM.Resources
import ArM.Character
import ArM.JSON
import Data.Aeson.Encode.Pretty
-- import Data.Aeson.Key
import qualified Data.ByteString.Lazy.Char8 as B



import Swish.RDF.Ruleset



testCharacter = "armchar:cieran"

main :: IO ()
main = do
        g <- getGraph characterFile armFile resourceFile


        DTIO.putStrLn $ formatGraphAsText $ g
        let vb = getCharacterMetadata g testCharacter 
        print vb

        let sheet0 = show $ fromJust $ getInitialSheet g testCharacter
        print sheet0

        print "Pregame Advancement"
        print $ getPregameAdvancements g testCharacter

        print "Ingame Advancement"
        print $ getIngameAdvancements g testCharacter

        let x = getInitialCS g testCharacter 
        print "Initial Sheet"
        print x

        B.putStrLn $ encodePretty x
