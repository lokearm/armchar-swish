{-# LANGUAGE OverloadedStrings #-}

module ArM.WebService where

import Web.Scotty  as S
import Network.HTTP.Types

import System.IO as IO
import Control.Monad
import qualified Data.Text.Lazy as  T
import Swish.RDF.Formatter.Turtle
import Swish.RDF.Graph
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import qualified ArM.Character.Advancement as A
import qualified ArM.Character.Character as C
import qualified ArM.CharacterQuery as CQ
import qualified ArM.CharacterMap as CM
import ArM.JSON
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Control.Concurrent.STM as STM
import qualified ArM.Resources as AR

jsonif Nothing = notfound404
jsonif (Just x) = json x


stateScotty ::  RDFGraph -> RDFGraph -> RDFGraph -> STM.TVar CM.MapState -> S.ScottyM ()
stateScotty g schema res stateVar = do

        get "/" $ do     
          text "Test a get call - available paths for get:\n  /    (this page)\n  /graph\n  /initial\n  /gamestart\n  /res\n  /schema\n"
        get "/schema" $ do     
          text $ T.fromStrict $ formatGraphAsText $ schema
        get "/res" $ do     
          text $ T.fromStrict $ formatGraphAsText $ res
        get "/graph" $ do     
          text $ T.fromStrict $ formatGraphAsText $ g
        get "/gamestart/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          jsonif $ C.getGameStartCharacter g char 
        get "/initial/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          jsonif $ C.getInitialCS g char 
        get "/test/adv/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          text $ T.pack $ show $ A.getIngameAdvancements g char
        get "/adv/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          json $ A.getIngameAdvancements g char
        get "/pregameadvancement/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          json $ A.getPregameAdvancements g char
        get "/cs/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (CM.CharacterRecord cgraph) -> do
                text $ T.fromStrict $ formatGraphAsText $ cgraph
             Nothing -> notfound404 
        get "/virtue/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (CM.CharacterRecord cgraph) -> do
                json $ CQ.getVirtues cgraph
             Nothing -> notfound404 
        get "/flaw/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (CM.CharacterRecord cgraph) -> do
                json $ CQ.getFlaws cgraph
             Nothing -> notfound404 
        get "/pt/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (CM.CharacterRecord cgraph) -> do
                json $ CQ.getPTs cgraph
             Nothing -> notfound404 
        get "/ability/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (CM.CharacterRecord cgraph) -> do
                json $ CQ.getAbilities cgraph
             Nothing -> notfound404 
        get "/characteristic/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (CM.CharacterRecord cgraph) -> do
                json $ CQ.getCharacteristics cgraph
             Nothing -> notfound404 

        S.delete "/" $ do
          html "This was a DELETE request!"  -- send 'text/html' response
        post "/" $ do
          text "This was a POST request!"
        put "/" $ do
          text "This was a PUT request!"

notfound404 = do status notFound404
                 text "404 Not Found."

getCSGraph stateVar = do
          (char, year, season) <- getParam
          cmap <- liftIO $ CM.stMap <$> STM.readTVarIO stateVar
          let r = CM.lookup cmap char season (read year)
          return r

getParam = do
          liftIO $ print "foobar"
          char' <- param  "char"
          let char = "armchar:" ++ char'
          liftIO $ print $ "char: " ++ char
          year <- param  "year"
          liftIO $ print $ "year: " ++ year
          season <- param  "season"
          liftIO $ print $ "season: " ++ season
          return (char, year, season)
