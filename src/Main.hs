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
import System.Environment
import System.Console.GetOpt

import ArM.Debug.Time
import ArM.Markdown.IO
import ArM.Types.MapState
import qualified ArM.Character.CharGen as TCG

import Data.Maybe (fromJust)


data Options = Options {
  sagaFile :: String,
  charFile :: Maybe String,
  outFile  :: String,
  debugFile  :: Maybe String,
  outputDir  :: Maybe String,
  advancementFile  :: Maybe String
}
defaultOptions :: Options
defaultOptions = Options {
  sagaFile = "Test/saga.ttl",
  charFile = Nothing,
  outFile  = "test.md",
  debugFile  = Nothing,
  outputDir  = Nothing,
  advancementFile  = Nothing
}




options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option ['o']     ["output"]  (ReqArg 
            (\arg opt -> return opt { outFile = arg })
            "FILE") "output file"
    , Option ['c']     ["character"] (OptArg 
            (\arg opt -> return opt { charFile = arg })
            "FILE") "character file"
    , Option ['s']     ["saga"] (ReqArg 
            (\arg opt -> return opt { sagaFile = arg })
            "FILE") "saga file"
    , Option ['O']     ["debug-output"] (OptArg 
            (\arg opt -> return opt { debugFile = arg })
            "FILE") "debug output"
    , Option ['D']     ["output-directory"] (OptArg 
            (\arg opt -> return opt { outputDir = arg })
            "FILE") "output directory"
    , Option ['A']     ["advancement-file"] (OptArg 
            (\arg opt -> return opt { advancementFile = arg })
            "FILE") "output directory"
    ]

getMaybeHandle :: Maybe String -> IO Handle
getMaybeHandle Nothing = return stdout
getMaybeHandle (Just f) = openFile f WriteMode

dirOpts :: Options -> Options
dirOpts opt | outputDir opt == Nothing = opt
      | otherwise = opt {
          outFile = d ++ "/character.md",
          debugFile = Just $ d ++ "/character.triples",
          advancementFile = Just $ d ++ "/advancement.md"
       }
       where d = fromJust $ outputDir opt

main :: IO ()
main = do 
     putStrLn "Starting: armchar-swish  ..."
     printTime
     args <- getArgs
     let (opt,_,_) = getOpt RequireOrder options args
     opt0 <- foldl (>>=) (return defaultOptions) opt

     main' $ dirOpts opt0

main' :: Options -> IO ()
main' opts | charFile opts == Nothing = do 
     st0 <- loadSaga $ sagaFile opts
     st1 <- loadChars st0
     let cs = foldr (:) [] $  cgMap st1
     _ <- mapM writeCG cs
     writeSaga (sagaFile opts ++ ".md") (saga st1) 

     let cov = covenant st1
     writeCovenant (sagaFile opts ++ "-covenant.md") cov
     writeAdv (Just $ sagaFile opts ++ "-advancement.md") cov

     return ()
main' opts | otherwise = do 
     printTime
     sagaobject <- loadSaga $ sagaFile opts
     chargen <- loadChar sagaobject $ fromJust $ charFile opts
     let char = head $ TCG.charSheets chargen
     let chargraph = TCG.sheetGraph char

     -- putStrLn $ show $ charGraph chargen

     writeSheet (outFile opts) chargraph
     printTime
     writeAdv (advancementFile opts) chargen
     printTime

     h2 <- getMaybeHandle $ debugFile opts
     hPutStrLn h2 $ show chargraph
     hClose h2

     return ()
