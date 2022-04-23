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



import ArM.Query
import ArM.Load
import ArM.Resources
import ArM.Character as C
import qualified ArM.CharacterQuery as CQ
import qualified ArM.CharacterMap as CM
import ArM.JSON
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

testCharacter = "armchar:cieran"

-- main :: IO ()
main = do 
     (g,schema,res) <- getGraph characterFile armFile resourceFile

     let cl =  C.getAllCS g testCharacter
     let st = map (\ x -> show (CM.getKey x) ++ "\n" ) cl
     putStrLn $ join st

     print $ formatGraphAsText $ schema
     print $ formatGraphAsText $ res

     print $ formatGraphAsText $ g

     let s = merge schema res
     let cmap = CM.insertListS s CM.empty $ cl
     -- let cmap = CM.insertList CM.empty $ cl

     -- print $ getGameStartCharacter g testCharacter 

     let r = CM.lookup cmap "armchar:cieran" "Summer" 1217
     let f (CM.CharacterRecord x) = x
     let g = f $ fromJust r
     print $ CQ.getAbilities g

