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
-- import ArM.Debug.Trace
import ArM.Types.MapState
import ArM.Markdown.CharacterSheet
import ArM.Character.CharGen
import ArM.Types.SheetObject
-- import ArM.Types.Character

import System.Environment
import System.Console.GetOpt

import Swish.RDF.Graph (RDFGraph)

data Options = Options {
  sagaFile :: String,
  charFile :: String,
  outFile  :: String,
  debugFile  :: Maybe String
}
defaultOptions :: Options
defaultOptions = Options {
  sagaFile = "Test/saga.ttl",
  charFile = "Test/cieran.ttl",
  outFile  = "test.md",
  debugFile  = Nothing
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
    , Option ['O']     ["debug-output"] (ReqArg 
            (\arg opt -> return opt { debugFile = Just arg })
            "FILE") "saga file"
    ]

getMaybeHandle :: Maybe String -> IO Handle
getMaybeHandle Nothing = return stdout
getMaybeHandle (Just f) = openFile f WriteMode


writeSheet :: String -> RDFGraph -> IO ()
writeSheet fn cg = do
     handle <- openFile fn WriteMode
     let p = hPutStrLn handle
     mapM_ p $ printSheetObject  $ getSheetObject cg
     hClose handle

main :: IO ()
main = do 
     putStrLn "Starting: armchar-swish  ..."
     printTime
     args <- getArgs
     let (opt,_,_) = getOpt RequireOrder options args
     opts <- foldl (>>=) (return defaultOptions) opt

     printTime
     sagaobject <- loadSaga $ sagaFile opts
     chargen <- loadChar sagaobject $ charFile opts
     let char = head $ charSheets chargen
     let chargraph = sheetGraph char

     writeSheet (outFile opts) chargraph
     printTime

     h2 <- getMaybeHandle $ debugFile opts
     hPutStrLn h2 $ show chargraph
     hClose h2

     return ()
