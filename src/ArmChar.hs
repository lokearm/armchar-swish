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

import ArM.Debug.Time
import ArM.Char.Character
import ArM.Char.Markdown

import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.Aeson (FromJSON,encode,decode)

import Data.ByteString.Lazy.UTF8 (fromString)

import qualified Data.ByteString.Lazy as LB

data Options = Options {
  sagaFile :: String,
  charFile :: Maybe String,
  outFile  :: String,
  debugFile  :: Maybe String,
  outputDir  :: Maybe String
}
defaultOptions :: Options
defaultOptions = Options {
  sagaFile = "Test/saga.ttl",
  charFile = Nothing,
  outFile  = "test.md",
  debugFile  = Nothing,
  outputDir  = Nothing
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
    ]

dirOpts :: Options -> Options
dirOpts opt | outputDir opt == Nothing = opt
      | otherwise = opt {
          outFile = d ++ "/character.md",
          debugFile = Just $ d ++ "/character.triples"
       }
       where d = fromJust $ outputDir opt

main :: IO ()
main = do 
     putStrLn "Starting: armchar ..."
     printTime
     args <- getArgs
     let (opt,_,_) = getOpt RequireOrder options args
     opt0 <- foldl (>>=) (return defaultOptions) opt

     main' $ dirOpts opt0

s :: String
s = "{ \"foo\" : \"string\", \"bar\" : 0 }"

decodeJSON :: FromJSON a => String -> Maybe a
decodeJSON = decode . fromString

kpl :: KeyPairList 
kpl = KeyPairList [ KeyPair "foo" (TextValue $ T.pack "test")
                  , KeyPair "bar" (IntValue 0)  ] 

c :: Character 
c = defaultCharacter { charID = "Cieran", 
   concept = defaultConcept {
      charGlance = KeyPairList [ KeyPair "Name" (TextValue $ T.pack "Cieran") ],
      charData = KeyPairList [ KeyPair "Size" (IntValue 0) ]
   }
   }
putStrLns :: [ String ] -> IO ()
putStrLns [] = return ()
putStrLns (x:xs) = putStrLn x >> putStrLns xs

main' :: Options -> IO ()
main' opts | charFile opts == Nothing = do 
     putStrLn $ show $ encode c
     t <- LB.readFile "test.json"
     let char = fromJust $ ( decode t  :: Maybe Character )
     putStrLn $ show char
     putStrLns $ printMD char
     putStrLns $ printMD $ prepareCharacter char
     return ()
main' _ | otherwise = error "Not implemented!" 
