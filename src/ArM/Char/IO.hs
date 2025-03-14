-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.IO
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Simple utilities to read and parse ArMChar files.
--
-- The functions in this module generally take a filename (String)
-- and returns a Maybe object by attempting to parse the relevant 
-- file.
--
-- Some objects, like the Spell Database, are stored in CSV, others
-- in JSON, including Character and Covenant.
--
-----------------------------------------------------------------------------
module ArM.Char.IO where

-- import qualified System.IO as IO -- for file IO
import Data.Maybe 
import Data.Aeson (decode)
import qualified Data.CSV as CSV
-- import Data.Text.Lazy.IO as I
import qualified Data.ByteString.Lazy as LB

import Text.ParserCombinators.Parsec

import ArM.Char.Character
import ArM.Char.Markdown
import ArM.Char.Saga
import ArM.Char.Spell
import ArM.BasicIO

import ArM.Debug.Trace

-- | Read a character from JSON.  Return Maybe Character
readCharacter :: String -- ^ Filename
              -> IO (Maybe Character)
readCharacter fn = LB.readFile fn 
            >>= return . prepMaybe . decode
   where prepMaybe Nothing = Nothing
         prepMaybe (Just x) = Just $ prepareCharacter x

-- | Read a saga from JSON.  Return Maybe Saga
readSagaFile :: String -- ^ Filename
             -> IO (Maybe SagaFile)
readSagaFile fn = LB.readFile fn >>= return . decode

readSaga :: String -- ^ Filename
         -> IO (Maybe Saga)
readSaga fn = readSagaFile fn >>= passMaybe loadSaga

passMaybe :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
passMaybe _ Nothing = return Nothing
passMaybe g (Just x) = fmap Just $ g x

-- | Read spells from CSV.  Return Maybe SpellDB.
readSpellDB :: String -- ^ Filename
              -> IO (Maybe SpellDB)
readSpellDB fn = parseFromFile CSV.csvFile fn >>= return . Just . spellDB . g
  where g (Left _) = [[]]
        g (Right x) = x

loadSaga :: SagaFile -> IO Saga
loadSaga saga = do
   db <- readSpellDB $ spellFile saga
   cs <- mapM readCharacter $ characterFiles saga
   trace (">> " ++ (show $ map (concept.fromJust) cs)) $ return   Saga { covenants = []  
           , characters = map fromJust $ filter (Nothing/=) cs
           , spells = fromJust db }

writeCharacter :: String -> SpellDB -> Character -> IO ()
writeCharacter dir db c = do
     writeLns fn $ printMDaug db c
     return ()
     where fn = dir ++ "/" ++ charID c ++ ".md"

writeCharacters :: String -> Saga -> IO ()
writeCharacters dir saga = mapM (writeCharacter dir db) (characters saga) >> return ()
     where db = spells saga
