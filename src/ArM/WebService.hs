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
-- to be defined in the `stateScotty` function which is called from
-- the `Main` module.
--
-- A range of auxiliary functions are defined to avoid code duplication.
-- Some also print diagnostic output like CPU time.
--
-----------------------------------------------------------------------------

module ArM.WebService (stateScotty) where

import Web.Scotty  as S
import Network.HTTP.Types

-- import Control.Monad
import qualified Data.Text.Lazy as  T
import Data.Text.Lazy.Builder (toLazyText)
import Swish.RDF.Formatter.Turtle (formatGraphAsText,formatGraphIndent)
import qualified Swish.RDF.Graph as G
import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Data.String (fromString)

import           ArM.Types.RDF (fromRDFGraph)
import qualified ArM.Types.Advancement as TA
import qualified ArM.Types.Character as TC
import qualified ArM.Types.Season as TS
import qualified ArM.Character.CharGen as TCG
import qualified ArM.CharacterQuery as CQ
import ArM.Resources()
import qualified Data.Aeson as Aeson

import qualified ArM.STM as STM

import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Middleware.Cors (simpleCors)

import System.CPUTime
import ArM.Time

-- TEST
-- import qualified ArM.Rules.Persistence as RP

showw :: (Show a1, Show a2) => [(a1, a2)] -> String
showw [] = ""
showw ((a,b):xs) = show a ++ "\n" ++ show b ++ "\n" ++ showw xs

stateScotty ::  STM.MapState -> S.ScottyM ()
stateScotty stateVar = do
        middleware logStdoutDev
        middleware simpleCors

  -- GET
        -- Top level graphs
        get "/" $ do     
          text $ "Test a get call\n"
        get "/saga" $ do     
          saga <- liftIO $ STM.getSaga stateVar 
          -- printGraph saga
          json saga
        get "/schema" $ do     
          schema <- liftIO $ STM.getSchemaGraph stateVar
          printGraph schema
        get "/res" $ do     
          res <- liftIO $ STM.getResourceGraph stateVar
          printGraph res
        get "/graph/:char" $ do     
          char <- param "char"
          cg <- liftIO $ STM.lookupCharIO stateVar char
          case (cg) of
             Nothing -> notfound404
             Just cg1 -> printGraph $ TCG.rawGraph cg1
        get "/show/chargen/:char" $ do     
          char <- param "char"
          cg <- liftIO $ STM.lookupCharIO stateVar char
          case (cg) of
             Nothing -> notfound404
             Just cg1 -> do
               let g = TCG.charSheets cg1
               let t1 = map ( TS.timeOf . TCG.advancement ) g
               let t2 = map ( TS.timeOf . TCG.sheetObject ) g
               let ts = zip t1 t2
               let as = map ( TCG.advancement ) g
               text $ T.pack $ showw $ zip ts as

        -- Advancement lists
        get "/show/adv/:char" $ do     
          char <- param "char"
          cg <- liftIO $ STM.lookupCharIO stateVar char
          case (cg) of
             Nothing -> notfound404
             Just cg1 -> do
               let g = TCG.charGraph cg1
               let clab = TCG.charID cg1
               text $ T.pack $ show $ sort $ TA.getIngameAdvancements g clab
        get "/adv/:char" $ do     
          char <- param "char"
          cg <- liftIO $ STM.lookupCharIO stateVar char
          case (cg) of
             Nothing -> notfound404
             Just cg1 -> do
               let g = TCG.charGraph cg1
               let clab = TCG.charID cg1
               json $ sort $ TA.getIngameAdvancements g clab
        get "/pregameadvancement/:char" $ do     
          char <- param "char"
          cg <- liftIO $ STM.lookupCharIO stateVar char
          case (cg) of
             Nothing -> notfound404
             Just cg1 -> do
               let g = TCG.charGraph cg1
               let clab = TCG.charID cg1
               json $ TA.getPregameAdvancements g clab

        -- Character Sheet
        get "/char/:char" $ do     
          char <- param "char"
          cg <- liftIO $ STM.lookupCharIO stateVar char
          case (cg) of
             Nothing -> notfound404
             Just cg1 -> do
               let g = TCG.charGraph cg1
               let clab = TCG.charID cg1
               let c =  fromRDFGraph g clab :: TC.Character
               json c
        get "/cs/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (cgraph) -> do
                text $ T.fromStrict $ formatGraphAsText $ cgraph
             Nothing -> notfound404 

        -- Traits
        getAb stateVar "/equipment/:char/:year/:season"   CQ.getItemList 
        getAb stateVar "/virtue/:char/:year/:season"      CQ.getVirtues 
        getAb stateVar "/flaw/:char/:year/:season"        CQ.getFlaws 
        getAb stateVar "/pt/:char/:year/:season"          CQ.getPTs 
        getAb stateVar "/ability/:char/:year/:season"     CQ.getAbilities 
        getAb stateVar "/spell/:char/:year/:season"       CQ.getSpells 
        getAb stateVar "/art/:char/:year/:season"         CQ.getArts 
        getAb stateVar "/combat/:char/:year/:season"      CQ.getCombat
        getAb stateVar "/char/:char/:year/:season"        CQ.getMetaData
        getAb stateVar "/characteristic/:char/:year/:season" 
                                                     CQ.getCharacteristics 

  -- Test
        S.delete "/" $ do
          html "This was a DELETE request!"  -- send 'text/html' response
        post "/" $ do
          text "This was a POST request!"

  -- PUT
        put "/" $ do
          text "This was a PUT request!"
        put "/debug/char" $ do
          text "This was a PUT request!"
        put "/debug/adv" $ do
          text "This was a PUT request!"

        put "/adv" $ do
          adv <- jsonData :: S.ActionM TA.Advancement 
          adv1 <- liftIO $ STM.cleanAdvancement stateVar adv
          case (adv1) of
             Right x -> error409 $ "Malformed input.\n" ++ x
             Left x -> do
                 newg <- liftIO $ STM.putAdvancement stateVar x
                 case (newg) of
                    Right m -> error409 $
                                 "Advancement could not be inserted.\n" ++ m
                    Left m -> text $ T.pack $ "OK! " ++ m
        put "/char" $ do
          char <- jsonData :: S.ActionM TC.Character 
          liftIO $ print char
          r <- liftIO $ STM.putCharacter stateVar char
          case (r) of
             Left m -> text $ T.pack $ "OK! " ++ m
             Right m -> error409 $ "Data could not be inserted.\n" ++ m

-- | Return a 404 error over HTTP, with a simple human readable error
-- message.
notfound404 :: S.ActionM ()
notfound404 = do status notFound404
                 text "404 Not Found."
error409 :: String -> S.ActionM ()
error409 msg = do status conflict409
                  text $ T.pack $ "400 Conflict.\n" ++ msg

-- | Get a character record from the STM State.
-- The record is selected by HTTP/GET parameters found in the monad.
getCSGraph :: STM.MapState -> S.ActionM (Maybe G.RDFGraph)
getCSGraph stateVar = do
          (char, year, season) <- getParam
          let t = TS.defaultCharTime { TS.charYear = Just $ read year, 
                                    TS.charSeason = season }
          liftIO $ STM.lookupIO stateVar char t

-- | Get the HTTP/GET parameters selecting a character sheet and
-- print diagnostic output.
getParam :: S.ActionM (String,String,String)
getParam = do
          char <- param  "char"
          liftIO $ print $ "char: " ++ char
          year <- param  "year"
          liftIO $ print $ "year: " ++ year
          season <- param  "season"
          liftIO $ print $ "season: " ++ season
          return (char, year, season)

-- | Given a character record and a function, show the result
-- of applying the function to the record as JSON.
-- The function also shows CPU time
jsonif' :: Aeson.ToJSON a => Maybe G.RDFGraph ->
           (G.RDFGraph -> a) -> S.ActionM ()
jsonif' Nothing _  = notfound404
jsonif' (Just xin) fin =  jsonif'' xin fin
  where jsonif'' (x) f = do
            t1 <- liftIO $ getCPUTime
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            json $ f x
            t2 <- liftIO $ getCPUTime
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"

-- | Given a character record and a function, show the result
-- of applying the function to the record as text.
-- The function also shows CPU time
textif' :: Show a => Maybe G.RDFGraph ->
           (G.RDFGraph -> a) -> S.ActionM ()
textif' Nothing _  = notfound404
textif' (Just xin) fin =  textif'' xin fin
  where textif'' (x) f = do
            t1 <- liftIO $ getCPUTime
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            text $ T.pack $ show $ f x
            t2 <- liftIO $ getCPUTime
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"

{-
-- | Output the given object as JSON over HTTP, or return a 404 error
-- if Nothing is given.
jsonif ::  Aeson.ToJSON a => Maybe a -> S.ActionM ()
jsonif Nothing = notfound404
jsonif (Just x) = do
            t1 <- liftIO $ getCPUTime
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            json x
            t2 <- liftIO $ getCPUTime
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"

-- | Format and output the given RDFGraph over HTTP, or return a 404
-- error if Nothing is given.
graphif :: Maybe TC.CharacterSheet -> S.ActionM ()
graphif Nothing = notfound404
graphif (Just x) = do
            t1 <- liftIO $ getCPUTime
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            printGraph $ makeRDFGraph x
            t2 <- liftIO $ getCPUTime
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"
-}

-- | Format and output the given RDFGraph over HTTP.
printGraph :: G.RDFGraph -> S.ActionM ()
printGraph = text . toLazyText .  formatGraphIndent "\n" True

-- | Generate get responses for trait subsets
-- Both JSON and text versions are created, prepending the path
-- by `/show` for the text version.
getAb :: (Show a, Aeson.ToJSON a) => STM.MapState ->
         String -> (G.RDFGraph -> a) -> S.ScottyM ()
getAb st property func = textAb st ("/show"++property) func 
                       >> jsonAb st property func
   where
      textAb s p f = get (fromString p) $ do     
                     r <- getCSGraph s
                     textif' r f
      jsonAb s p f = get (fromString p) $ do     
                     r <- getCSGraph s
                     jsonif' r f
