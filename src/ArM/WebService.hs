{-# LANGUAGE OverloadedStrings #-}

module ArM.WebService (stateScotty) where

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

import System.CPUTime
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )

import Numeric

jsonif' r f = case (r) of
              Nothing -> notfound404
              (Just x) -> jsonif'' x f

jsonif'' (CM.CharacterRecord x) f = do
            t1' <- liftIO $ getCPUTime
            let t1 = fromIntegral ( t1' `div` 10^9 ) * 10**(-3)
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            json $ f x
            t2' <- liftIO $ getCPUTime
            let t2 = fromIntegral ( t2' `div` 10^9 ) * 10**(-3)
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"

jsonif Nothing = notfound404
jsonif (Just x) = do
            t1' <- liftIO $ getCPUTime
            let t1 = fromIntegral ( t1' `div` 10^9 ) * 10**(-3)
            liftIO $ print $ "Serving request (" ++ showf t1 ++ "s)"
            json x
            t2' <- liftIO $ getCPUTime
            let t2 = fromIntegral ( t2' `div` 10^9 ) * 10**(-3)
            liftIO $ print $ "CPUTime spent: " ++ showf (t2-t1) ++ "s (" ++ showf t1 ++ "s)"

showf f = showFFloat (Just 3) f "" 

stateScotty ::  RDFGraph -> RDFGraph -> RDFGraph -> STM.TVar CM.MapState -> S.ScottyM ()
stateScotty g schema res stateVar = do
        middleware logStdoutDev

        get "/" $ do     
          text "Test a get call - available paths for get:\n  /    (this page)\n  /graph\n  /initial\n  /gamestart\n  /res\n  /schema\n"
        get "/schema" $ do     
          text $ T.fromStrict $ formatGraphAsText $ schema
        get "/res" $ do     
          text $ T.fromStrict $ formatGraphAsText $ res
          liftIO $ print "FOOBAR"
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
          jsonif $ Just $ A.getIngameAdvancements g char
        get "/pregameadvancement/:char" $ do     
          char' <- param "char"
          let char = AR.armcharRes char'
          jsonif $ Just $ A.getPregameAdvancements g char
        get "/cs/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          case (r) of
             Just (CM.CharacterRecord cgraph) -> do
                text $ T.fromStrict $ formatGraphAsText $ cgraph
             Nothing -> notfound404 
        get "/virtue/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getVirtues 
        get "/flaw/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getFlaws 
        get "/pt/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getPTs 
        get "/ability/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getAbilities 
        get "/characteristic/:char/:year/:season" $ do     
          r <- getCSGraph stateVar
          jsonif' r CQ.getCharacteristics 

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
          char' <- param  "char"
          let char = "armchar:" ++ char'
          liftIO $ print $ "char: " ++ char
          year <- param  "year"
          liftIO $ print $ "year: " ++ year
          season <- param  "season"
          liftIO $ print $ "season: " ++ season
          return (char, year, season)
