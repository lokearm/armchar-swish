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

import ArM.CharGraph
import ArM.Character.CharGen

sagaFile :: String
sagaFile = "Test/saga.ttl"
charFile :: String
charFile = "Test/cieran.ttl"

main :: IO ()
main = do 
     putStrLn "Starting: armchar-swish  ..."
     sagaobject <- loadSaga sagaFile
     chargen <- loadChar sagaobject charFile
     let char = head $ charSheets chargen
     let chargraph = sheetGraph char

     putStrLn $ show chargraph
