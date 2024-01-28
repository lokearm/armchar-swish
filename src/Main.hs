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

import System.Environment
import System.Console.GetOpt

data Options = Options {
  sagaFile :: String,
  charFile :: String,
  outFile  :: String
}
defaultOptions :: Options
defaultOptions = Options {
  sagaFile = "Test/saga.ttl",
  charFile = "Test/cieran.ttl",
  outFile  = "test.md"
}




options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option ['o']     ["output"]  (ReqArg 
            (\arg opt -> return opt { outFile = arg })
            "FILE") "output file"
    , Option ['c']     ["character"] (ReqArg 
            (\arg opt -> return opt { charFile = arg })
            "FILE") "character file"
    , Option ['s']     ["saga"] (ReqArg 
            (\arg opt -> return opt { sagaFile = arg })
            "FILE") "saga file"
    ]


main :: IO ()
main = do 
     putStrLn "Starting: armchar-swish  ..."
     printTime
     args <- getArgs
     let (opt,_,_) = getOpt RequireOrder options args
     opts <- foldl (>>=) (return defaultOptions) opt

     sagaobject <- loadSaga $ sagaFile opts
     chargen <- loadChar sagaobject $ charFile opts
     let char = head $ charSheets chargen
     let chargraph = sheetGraph char

     handle <- openFile (outFile opts) WriteMode

     let p = hPutStrLn handle

     mapM_ p $ printSheetObject  $ getSheetObject chargraph
     hClose handle

     return ()
