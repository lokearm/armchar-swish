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

-- | Saga File
sagaFile :: String
sagaFile = "Test/saga.ttl"

main :: IO ()
main = do 
     putStrLn "Starting: armchar-swish  ..."
     printTime
     putStrLn "Not implemented"
