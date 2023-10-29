{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2023: Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
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
     putStrLn ""
     mapM_ putStrLn $ printVirtues  chargraph
     putStrLn ""
     mapM_ putStrLn $ printFlaws chargraph
     putStrLn ""
     mapM_ putStrLn $ printAbilities chargraph
     putStrLn ""
     mapM_ putStrLn $ printArts chargraph
     putStrLn ""
     -- mapM_ putStrLn $ debugArts chargraph
