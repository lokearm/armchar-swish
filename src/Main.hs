{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2022: Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- This is the main module for the armchar web server.
-- This includes some wiring to reduce inter-dependencies between
-- other modules, and some configuration options which may later be
-- moved to data files or command line parameters.
--
-- * The data structure is loaded by the `getRawGraph` function
--   defined in `ArM.Load`
--
-----------------------------------------------------------------------------

module Main where

-- Timer
import ArM.Time
import ArM.CharGraph
import ArM.Markdown.CharacterSheet
import ArM.Character.CharGen
-- import ArM.Types.Character

-- | Saga File
sagaFile :: String
sagaFile = "Test/saga.ttl"
charFile :: String
charFile = "Test/cieran.ttl"

main :: IO ()
main = do 
     putStrLn "Starting: armchar-swish  ..."
     printTime
     sagaobject <- loadSaga sagaFile
     chargen <- loadChar sagaobject charFile
     let char = head $ charSheets chargen
     let chargraph = sheetGraph char
     mapM_ putStrLn $ printMetaData  chargraph
     mapM_ putStrLn $ printVirtues  chargraph
     mapM_ putStrLn $ printFlaws chargraph
     mapM_ putStrLn $ printAbilities chargraph
     mapM_ putStrLn $ printArts chargraph
     -- mapM_ putStrLn $ debugArts chargraph
