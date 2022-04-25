{-# LANGUAGE OverloadedStrings #-}
-- (C) 2022: Hans Georg Schaathun <hg+gamer@schaathun.net>

module Main where

import System.IO as IO
import Control.Monad
import qualified Data.Text.Lazy as  T
import Swish.RDF.Formatter.Turtle
import Swish.RDF.Graph
import Data.Maybe
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)


-- Web service
import qualified Web.Scotty  as S
import ArM.WebService

-- import ArM.Query
import ArM.Load
import ArM.Resources
import ArM.Character.Character as C
import qualified ArM.CharacterQuery as CQ
import qualified ArM.CharacterMap as CM
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Control.Concurrent.STM as STM


-- Auth
import Network.Wai.Middleware.HttpAuth
import Data.SecureMem -- for constant-time comparison

authf u p = return $ u == "user" && secureMemFromByteString p == password
password :: SecureMem
password = secureMemFromByteString "ElksRun" 


testCharacter = armcharRes "cieran"

-- main :: IO ()
main = do 
     (g,schema,res) <- getGraph characterFile armFile resourceFile


     print $ formatGraphAsText $ schema
     print $ formatGraphAsText $ res
     print $ formatGraphAsText $ g

     let s = merge schema res
     let cl =  C.getAllCS g testCharacter
     let cmap = CM.insertListS s CM.empty $ cl
     -- let cmap = CM.insertList CM.empty $ cl
     stateVar <- STM.newTVarIO CM.MapState { CM.stMap = cmap }

     print $ getGameStartCharacter g $ armcharRes "cieran" 

     -- print $ encodePretty $ A.getIngameAdvancements g testCharacter

     -- let r = CM.lookup cmap "armchar:cieran" "Summer" 1217
     -- let f (CM.CharacterRecord x) = x
     -- let g = f $ fromJust r
     -- print $ CQ.getAbilities g

     let st = map (\ x -> show (CM.getKey x) ++ "\n" ) cl
     putStrLn $ join st

     print "Starting Scotty"
     S.scotty 3000 $ stateScotty g schema res stateVar

        -- middleware $ basicAuth authf "armchar"
