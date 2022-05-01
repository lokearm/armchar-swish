{-# LANGUAGE OverloadedStrings #-}
-- (C) 2022: Hans Georg Schaathun <hg+gamer@schaathun.net>

module Main where

import System.IO as IO
import qualified Data.Text.IO as DTIO
import qualified Data.Text.Lazy.IO as DTLIO
import Control.Monad
import qualified Data.Text.Lazy as  T
import Swish.RDF.Formatter.Turtle
import Swish.RDF.Graph
import Data.Maybe
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import Data.Text.Encoding
import Network.URI
import ArM.Resources


import qualified Data.ByteString.Lazy as BS
import ArM.Load
import ArM.Resources as AR
import ArM.Character
import ArM.Types.Character
import ArM.STM
import ArM.JSON
import Data.Aeson.Encode.Pretty
import Data.Aeson

testCharacter = "armchar:cieran"

s = "{\n     \"arm:hasDescription\": \"<p>You automatically master every spell that you learn. All your spells start with a score of 1 in the corresponding Ability. You may choose a different special ability for every spell you have. Further, all experience points you put into Spell Mastery Abilities are doubled.</p>  \",\n     \"arm:hasLabel\": \"Flawless Magic\",\n     \"arm:hasReference\": \"[ArM5:42]\",\n        \"arm:prefixedid\": \"_:118\", \n      \"arm:traitClass\": { \n     \"prefixedid\": \"armr:flawlessMagic\" \n   } \n } \n"

-- main :: IO ()
main = do 

     (g,schema,res) <- getGraph AR.characterFile AR.armFile AR.resourceFile
     contents <- BS.readFile "Test/adv.json"
     let  adv :: Maybe Advancement
          adv = decode contents
     print adv
     let  advg = merge schema $ makeRDFGraph $ fromJust adv
     DTIO.putStrLn $ formatGraphAsText $ advg
     print "persistGraph"
     DTIO.putStrLn $ formatGraphAsText $ persistGraph $ advg
     print "persistGraph'"
     DTIO.putStrLn $ formatGraphAsText $ persistGraph' $ advg
     print "end"

