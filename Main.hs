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

-- Web service
import Web.Scotty  as S
import Network.HTTP.Types

import ArM.Query
import ArM.Load
import ArM.Resources
import ArM.Character
import ArM.JSON
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B



import Swish.RDF.Ruleset



testCharacter = "armchar:cieran"

-- main :: IO ()
main = do 
     g <- getGraph characterFile armFile resourceFile

     print "Starting"

     scotty 3000 $ do
        get "/" $ do     
          text "Test a get call - available paths for get:\n  /    (this page)\n  /graph\n  /initial\n  /gamestart\n"
        get "/graph" $ do     
          text $ T.fromStrict $ formatGraphAsText $ g
        get "/gamestart" $ do     
          json $ getGameStartCharacter g testCharacter 
        get "/initial" $ do     
          json $ getInitialCS g testCharacter 
        S.delete "/" $ do
          html "This was a DELETE request!"  -- send 'text/html' response
        post "/" $ do
          text "This was a POST request!"
        put "/" $ do
          text "This was a PUT request!"
