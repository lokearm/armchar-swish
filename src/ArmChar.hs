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
import ArM.Char.IO
import ArM.Char.Character
import ArM.Char.Advancement
import ArM.Char.Markdown
import ArM.Char.Saga

-- import Data.Aeson (decode)

-- import qualified Data.Text as T
import Data.Maybe (fromJust)

-- import qualified Data.ByteString.Lazy as LB

data Options = Options 
  { sagaFile :: Maybe String
  , charFile :: Maybe String
  , outFile  :: Maybe String
  , jsonFile :: Maybe String
  , spellDBFile :: Maybe String
  , debugFile  :: Maybe String
  , seasonFile  :: Maybe String
  , gameStartDir  :: Maybe String
  , currentDir  :: Maybe String
  , advanceSeason  :: Maybe String
} deriving (Show)
defaultOptions :: Options
defaultOptions = Options 
  { sagaFile = Just "Test/saga.ttl"
  , charFile = Nothing
  , outFile  = Nothing
  , jsonFile  = Nothing
  , spellDBFile = Nothing
  , debugFile  = Nothing
  , seasonFile  = Nothing
  , gameStartDir  = Nothing
  , currentDir  = Nothing
  , advanceSeason  = Nothing
}


options :: [ OptDescr (Options -> Options) ]
options =
    [ Option ['c']     ["character"] (ReqArg 
            (\arg opt -> opt { charFile = Just arg })
            "FILE") "character file"
    , Option ['D']     ["current-dir"] (ReqArg 
            (\arg opt -> opt { currentDir = Just arg })
            "FILE") "directory for current character sheets"
    , Option ['g']     ["game-start-dir"] (ReqArg 
            (\arg opt -> opt { gameStartDir = Just arg })
            "FILE") "JSON output file"
    , Option ['j']     ["json"] (ReqArg 
            (\arg opt -> opt { jsonFile = Just arg })
            "FILE") "JSON output file"
    , Option ['o']     ["output"]  (ReqArg 
            (\arg opt -> opt { outFile = Just arg })
            "FILE") "output file"
    , Option ['O']     ["debug-output"] (ReqArg 
            (\arg opt -> opt { debugFile = Just arg })
            "FILE") "debug output"
    , Option ['s']     ["saga"] (ReqArg 
            (\arg opt -> opt { sagaFile = Just arg })
            "FILE") "saga file"
    , Option ['S']     ["spells"] (ReqArg 
            (\arg opt -> opt { spellDBFile = Just arg })
            "FILE") "input file for spell database"
    , Option ['t']     ["advance-to"] (ReqArg 
            (\arg opt -> opt { advanceSeason = Just arg })
            "SEASON") "advance to "
    , Option ['T']     ["sheet"] (ReqArg 
            (\arg opt -> opt { seasonFile = Just arg })
            "FILE") "output file for current character sheet"
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
     (opt,n) <- armcharOpts args
     putStrLns n
     putStrLn $ "Options: " ++ show opt

     main' opt

advChar :: Maybe String -> Maybe String -> Character -> IO ()
advChar Nothing _ _ = return ()
advChar sn fn cs0 = do
     writeMaybeOList fn $ printMD cs
     return ()
   where seasn = parseSeasonTime sn
         cs = advanceCharacter seasn cs0

advSaga :: Maybe String -> Maybe String -> Saga -> IO ()
advSaga Nothing _ _ = return ()
advSaga _ Nothing _ = return ()
advSaga sn (Just dir) s1 = writeCurrent dir s2
   where seasn = parseSeasonTime sn
         s2 = advanceSaga seasn s1

main' :: Options -> IO ()
main' opts | charFile opts /= Nothing = do 
     putStrLn $ "Reading file " ++ fn
     t <- readCharacter fn
     db' <- readSpell $ spellDBFile opts
     let char = fromJust t  
     let db = fromJust db'
     let cs = prepareCharacter char
     writeMaybeOList ( debugFile opts ) $ printMDaug db char
     writeMaybeOList ( outFile opts ) $ printMDaug db cs
     writeMaybeJSON ( jsonFile opts ) cs 
     advChar ( advanceSeason opts ) (seasonFile opts) cs
     return ()
   where fn = fromJust $ charFile opts
         readSpell Nothing = return Nothing
         readSpell (Just f) = readSpellDB f
main' opts | sagaFile opts /= Nothing = do 
     saga <- readSaga $ fromJust $ sagaFile opts
     case saga of
        Nothing -> error "Could not read Saga file"
        (Just s1) -> do
               writeGameStart gsf  s1
               writeLns (gsf ++ "/errors.md") $ 
                  "# Errors in Character Design":"":pregameErrors s1
               advSaga ( advanceSeason opts ) (currentDir opts ) s1
               return ()
        where gsf = fromJust $ gameStartDir opts
main' _ | otherwise = error "Not implemented!" 
