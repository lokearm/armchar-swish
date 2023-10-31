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

import System.IO -- for file IO

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
outFile :: String
outFile = "test.md"

main :: IO ()
main = do 
     putStrLn "Starting: armchar-swish  ..."
     printTime
     sagaobject <- loadSaga sagaFile
     chargen <- loadChar sagaobject charFile
     let char = head $ charSheets chargen
     let chargraph = sheetGraph char

     handle <- openFile outFile WriteMode

     let p = hPutStrLn handle

     mapM_ p $ printMetaData  chargraph
     mapM_ p $ printChar  chargraph
     mapM_ p $ printMisc  chargraph

     p ""
     mapM_ p $ printVF  chargraph

     p ""
     mapM_ p $ printAbilities chargraph
     p ""
     mapM_ p $ printArts chargraph
     p ""
     mapM_ p $ printSpells chargraph
     hClose handle

     return ()
