{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.WebService
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- This defines the Web API using `Scotty`.  Every URL pattern has 
-- to be defined in this file.
--
-----------------------------------------------------------------------------

module ArM.WebService (stateScotty) where

import Web.Scotty  as S
import Network.HTTP.Types

import Control.Monad
import qualified Data.Text.Lazy as  T
import Data.Text.Lazy.Builder (toLazyText)
import Swish.RDF.Formatter.Turtle (formatGraphAsText,formatGraphIndent)
import qualified Swish.RDF.Graph as G
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.List (sort)

import           ArM.Query.Metadata (getMetaData)
import qualified ArM.Types.Character as TC
import qualified ArM.Character as C
import qualified ArM.CharacterQuery as CQ
import qualified ArM.Resources as AR
import ArM.JSON

import           ArM.STM 
import qualified Control.Concurrent.STM as STM

import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Middleware.Cors (simpleCors)

import System.CPUTime
import ArM.Time

-- TEST
import qualified ArM.Rules.Persistence as RP

stateScotty ::  STM.TVar MapState -> S.ScottyM ()
stateScotty stateVar = do
        middleware logStdoutDev
        middleware simpleCors

  -- GET
        -- Top level graphs
        get "/" $ do     
          text $ "Test a get call\n"
        get "/schema" $ do     
          schema <- liftIO $ getSchemaGraph stateVar
          printGraph schema
        get "/res" $ do     
          res <- liftIO $ getResourceGraph stateVar
          printGraph res
        get "/graph" $ do     
          g <- liftIO $ getStateGraph stateVar
          printGraph  g

        -- Pre-defined character sheets
        get "/graph/gamestart/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          g <- liftIO $ getStateGraph stateVar
          graphif $ C.getGameStartCharacter g char 
        get "/gamestart/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          g <- liftIO $ getStateGraph stateVar
          jsonif $ C.getGameStartCharacter g char 

        -- Advancement lists
        get "/show/adv/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          g <- liftIO $ getStateGraph stateVar
          text $ T.pack $ show $ sort $ C.getIngameAdvancements g char
        get "/adv/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          g <- liftIO $ getStateGraph stateVar
          jsonif $ Just $ sort $ C.getIngameAdvancements g char
        get "/pregameadvancement/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          g <- liftIO $ getStateGraph stateVar
          jsonif $ Just $ C.getPregameAdvancements g char

        -- Character Sheet
        get "/char/:char" $ do     
          char <- fmap AR.armcharRes $ param "char"
          g <- liftIO $ getStateGraph stateVar
          let c =  TC.fromRDFGraph g char :: TC.Character
          json c
        get "/char/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (CharacterRecord cgraph) -> do
                json $ getMetaData cgraph
             Nothing -> notfound404 
        get "/cs/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (CharacterRecord cgraph) -> do
                text $ T.fromStrict $ formatGraphAsText $ cgraph
             Nothing -> notfound404 

        -- Traits
        get "/virtue/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getVirtues 
        get "/test/virtue/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          textif' r CQ.getVirtues 
        get "/flaw/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getFlaws 
        get "/test/flaw/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          textif' r CQ.getFlaws 
        get "/pt/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getPTs 
        get "/test/pt/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          textif' r CQ.getPTs 
        get "/ability/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getAbilities 
        get "/test/ability/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          textif' r CQ.getAbilities 
        get "/spell/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getSpells 
        get "/test/spell/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          textif' r CQ.getSpells 
        get "/art/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getArts 
        get "/test/art/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          textif' r CQ.getArts 
        get "/characteristic/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getCharacteristics 
        get "/test/characteristic/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          textif' r CQ.getCharacteristics 

  -- Test
        S.delete "/" $ do
          html "This was a DELETE request!"  -- send 'text/html' response
        post "/" $ do
          text "This was a POST request!"

  -- PUT
        put "/" $ do
          text "This was a PUT request!"
        put "/debug/adv" $ do
          adv <- jsonData :: S.ActionM C.Advancement 
          st <- liftIO $ STM.readTVarIO stateVar
          let g = charRawGraph st
          let schema = schemaGraph st
          let advg = TC.makeRDFGraph adv
          let g1 = RP.persistGraph schema advg
          let g0 = RP.persistedGraph (charGraph st) (TC.rdfid adv) 
          let gg = (g0 `G.delete` g) `G.addGraphs` g1
          liftIO $ print $ formatGraphIndent "\n" True advg
          liftIO $ print $ formatGraphIndent "\n" True g1
          liftIO $ print $ formatGraphIndent "\n" True g0
          let newst = st `updateGraph` gg
          case (newst) of
                Left s -> do
                   liftIO $ STM.atomically $ STM.writeTVar stateVar s 
                   printGraph gg
                Right x -> text $ T.pack x

        put "/adv" $ do
          adv <- jsonData :: S.ActionM C.Advancement 
          newg <- liftIO $ putAdvancement stateVar adv
          liftIO $ print adv
          case (newg) of
             Left g -> printGraph g
             Right x -> text $ T.pack x
        put "/char" $ do
          char <- jsonData :: S.ActionM TC.Character 
          liftIO $ print char
          newg <- liftIO $ putCharacter stateVar char
          case (newg) of
             Left g -> printGraph g
             Right x -> text $ T.pack x

notfound404 = do status notFound404
                 text "404 Not Found."

getCSGraph stateVar = do
          (char, year, season) <- getParam
          r <- liftIO $ ArM.STM.lookup stateVar char season (read year)
          return r

getParam = do
          char <- param  "char"
          liftIO $ print $ "char: " ++ char
          year <- param  "year"
          liftIO $ print $ "year: " ++ year
          season <- param  "season"
          liftIO $ print $ "season: " ++ season
          return (char, year, season)

jsonif' Nothing _  = notfound404
jsonif' (Just x) f =  jsonif'' x f

jsonif'' (CharacterRecord x) f = do
            t1 <- liftIO $ getCPUTime
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            json $ f x
            t2 <- liftIO $ getCPUTime
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"

textif' Nothing _  = notfound404
textif' (Just x) f =  textif'' x f
textif'' (CharacterRecord x) f = do
            t1 <- liftIO $ getCPUTime
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            text $ T.pack $ show $ f x
            t2 <- liftIO $ getCPUTime
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"

jsonif Nothing = notfound404
jsonif (Just x) = do
            t1 <- liftIO $ getCPUTime
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            json x
            t2 <- liftIO $ getCPUTime
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"

graphif Nothing = notfound404
graphif (Just x) = do
            t1 <- liftIO $ getCPUTime
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            printGraph $ C.makeRDFGraph x
            t2 <- liftIO $ getCPUTime
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"

-- printGraph = text . T.fromStrict . formatGraphAsText  
printGraph = text . toLazyText .  formatGraphIndent "\n" True
