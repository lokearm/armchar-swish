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

import System.Environment
import System.Console.GetOpt

import ArM.BasicIO
import ArM.Debug.Time
import ArM.Char.Character
import ArM.Char.Markdown

import Data.Aeson (decode)

-- import qualified Data.Text as T
import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy as LB

data Options = Options {
  sagaFile :: Maybe String,
  charFile :: Maybe String,
  outFile  :: Maybe String,
  debugFile  :: Maybe String
} deriving (Show)
defaultOptions :: Options
defaultOptions = Options {
  sagaFile = Just "Test/saga.ttl",
  charFile = Nothing,
  outFile  = Nothing,
  debugFile  = Nothing
}


options :: [ OptDescr (Options -> Options) ]
options =
    [ Option ['o']     ["output"]  (OptArg 
            (\arg opt -> opt { outFile = arg })
            "FILE") "output file"
    , Option ['c']     ["character"] (OptArg 
            (\arg opt -> opt { charFile = arg })
            "FILE") "character file"
    , Option ['s']     ["saga"] (OptArg 
            (\arg opt -> opt { sagaFile = arg })
            "FILE") "saga file"
    , Option ['O']     ["debug-output"] (OptArg 
            (\arg opt -> opt { debugFile = arg })
            "FILE") "debug output"
    ]

armcharOpts :: [String] -> IO (Options, [String])
armcharOpts argv =
      case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: armchar [OPTION...] "

main :: IO ()
main = do 
     putStrLn "Starting: armchar ..."
     printTime
     args <- getArgs
     putStrLn "Arguments"
     putStrLns args
     (opt,n) <- armcharOpts args
     putStrLn "Options"
     putStrLn $ show opt

     main' opt

main' :: Options -> IO ()
main' opts | charFile opts == Nothing = do 
     putStrLn $ "Reading file " ++ fn
     t <- LB.readFile fn
     let char = fromJust $ ( decode t  :: Maybe Character )
     putStrLn $ show char
     writeMaybeFile ( debugFile opts ) $ printMD char
     writeMaybeFile ( outFile opts ) $ printMD $ prepareCharacter char
     return ()
   where fn = fromJust $ charFile opts
main' _ | otherwise = error "Not implemented!" 
