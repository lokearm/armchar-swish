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
import ArM.Debug.Trace
-- import ArM.Debug.Trace
import ArM.Types.MapState
import ArM.Markdown.CharacterSheet
import ArM.Markdown.AdvancementLog
import qualified ArM.Character.CharGen as TCG
import ArM.Types.SheetObject
-- import ArM.Types.Character

import System.Environment
import System.Console.GetOpt

import Swish.RDF.Graph (RDFGraph)

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


writeSheet :: String -> RDFGraph -> IO ()
writeSheet fn cg = do
     handle <- openFile fn WriteMode
     let p = hPutStrLn handle
     mapM_ p $ printSheetObject  $ getSheetObject cg
     hClose handle
writeAdv :: Maybe String -> TCG.CharGen -> IO ()
writeAdv Nothing _ = return ()
writeAdv (Just fn) cg = do
     handle <- openFile fn WriteMode
     let p = hPutStrLn handle
     mapM_ p $ printAdvancementLog  as
     hClose handle
     where as = map TCG.advancement $ TCG.charSheets cg

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

writeCG :: TCG.CharGen -> IO ()
writeCG cg = trace ("Writing " ++ fn) $ writeSheet fn g
     where g = (TCG.sheetGraph . head . TCG.charSheets) cg
           fn = TCG.charFile cg ++ ".md"
