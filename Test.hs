{-# LANGUAGE OverloadedStrings #-}
-- (C) 2022: Hans Georg Schaathun <hg+gamer@schaathun.net>

module Main where

import System.IO as IO
import qualified Data.Text.IO as DTIO
-- import Data.Text.Lazy.IO as DTLIO
import Control.Monad
import qualified Data.Text.Lazy as  T
import Swish.RDF.Formatter.Turtle
import Swish.RDF.Graph
import Data.Maybe
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import Network.URI
import ArM.Resources


import ArM.Character.Trait
import ArM.JSON
import Data.Aeson.Encode.Pretty
import Data.Aeson

testCharacter = "armchar:cieran"

s = "{\n     \"arm:hasDescription\": \"<p>You automatically master every spell that you learn. All your spells start with a score of 1 in the corresponding Ability. You may choose a different special ability for every spell you have. Further, all experience points you put into Spell Mastery Abilities are doubled.</p>  \",\n     \"arm:hasLabel\": \"Flawless Magic\",\n     \"arm:hasReference\": \"[ArM5:42]\",\n        \"arm:prefixedid\": \"_:118\", \n      \"arm:traitClass\": { \n     \"prefixedid\": \"armr:flawlessMagic\" \n   } \n } \n"

-- main :: IO ()
main = do 

     -- print "JSON Source"
     -- print s
     -- print "Decoded"
     -- let dec = (decode s :: Maybe Trait)
     -- print dec 
     -- print "Reencoded"
     -- let enc = encode dec
     -- print enc
     -- print "Redecoded"
     -- print $ (decode enc :: Maybe Trait)

     let l1 = toRDFLabel $ fromJust $ parseURI "https://hg.schaathun.net/armchar/character/cieran"
     let l2 = armcharRes "cieran"

     print l1
     print l2
     print $ l1 == l2
