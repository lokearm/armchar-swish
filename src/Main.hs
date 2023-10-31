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
import ArM.Debug.Time
import ArM.CharGraph
import ArM.Markdown.CharacterSheet
import ArM.Character.CharGen
import ArM.Markdown.SheetObject
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

     mapM_ p $ printSheetObject  $ getSheetObject chargraph
     hClose handle

     return ()
