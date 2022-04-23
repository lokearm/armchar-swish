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


-- Web service
import Web.Scotty  as S
import Network.HTTP.Types


import ArM.Query
import ArM.Load
import ArM.Resources
import ArM.Character as C
import qualified ArM.CharacterMap as CM
import ArM.JSON
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

-- Auth
import Network.Wai.Middleware.HttpAuth
import Data.SecureMem -- for constant-time comparison

authf u p = return $ u == "user" && secureMemFromByteString p == password
password :: SecureMem
password = secureMemFromByteString "ElksRun" 


testCharacter = "armchar:cieran"

-- main :: IO ()
main = do 
     (g,schema,res) <- getGraph characterFile armFile resourceFile

     let cmap = CM.insertList CM.empty $ C.getAllCS g testCharacter

     print "Starting"
     -- print $ getGameStartCharacter g testCharacter 

     scotty 3000 $ do
        -- middleware $ basicAuth authf "armchar"

        get "/" $ do     
          text "Test a get call - available paths for get:\n  /    (this page)\n  /graph\n  /initial\n  /gamestart\n  /res\n  /schema\n"
        get "/schema" $ do     
          text $ T.fromStrict $ formatGraphAsText $ schema
        get "/res" $ do     
          text $ T.fromStrict $ formatGraphAsText $ res
        get "/graph" $ do     
          text $ T.fromStrict $ formatGraphAsText $ g
        get "/gamestart" $ do     
          json $ getGameStartCharacter g testCharacter 
        get "/initial" $ do     
          json $ getInitialCS g testCharacter 
        get "/cs/:char/:year/:season" $ do     
          liftIO $ print "foobar"
          char <- param "char"
          liftIO $ print $ "char: " ++ char
          year <- param "year"
          liftIO $ print $ "year: " ++ year
          season <- param "season"
          liftIO $ print $ "season: " ++ season
          let r = CM.lookup cmap char season (read year)
          case (r) of
             Just (CM.CharacterRecord cgraph) -> do
                text $ T.fromStrict $ formatGraphAsText $ cgraph
             Nothing -> do
                status notFound404
                text "404 Not Found."

        S.delete "/" $ do
          html "This was a DELETE request!"  -- send 'text/html' response
        post "/" $ do
          text "This was a POST request!"
        put "/" $ do
          text "This was a PUT request!"
