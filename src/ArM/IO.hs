-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.IO
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
module ArM.IO where

-- import qualified System.IO as IO -- for file IO
import Data.Maybe 
import Data.Aeson (decode)
import qualified Data.CSV as CSV
import qualified Data.ByteString.Lazy as LB

import Text.ParserCombinators.Parsec

import ArM.Char.Character
import ArM.Char.Markdown
import ArM.Cov.Saga
import ArM.Char.Spell
import ArM.BasicIO
import ArM.Helper

-- import ArM.Debug.Trace

-- |
-- = Read Saga Files
-- | Read a saga from JSON.  Return Maybe SagaFile
readSagaFile :: String -- ^ Filename
             -> IO (Maybe SagaFile)
readSagaFile fn = LB.readFile fn >>= return . decode

-- | Load the saga and its constituent objects from the given file.
-- This calls both `readSagaFile` and `loadSaga`
readSaga :: String -- ^ Filename
         -> IO (Maybe Saga)
readSaga fn = readSagaFile fn >>= passMaybe loadSaga



-- | Load constituent objects for a saga.
loadSaga :: SagaFile -> IO Saga
loadSaga saga = do
   db <- readSpellDB $ spellFile saga
   cs <- mapM readCharacter $ characterFiles saga
   return
     $ advanceSaga saga
     $ Saga { covenants = []  
           , gameStartCharacters = map fromJust $ filter (Nothing/=) cs
           , currentCharacters = []
           , spells = fromJust db }

-- |
-- == Read Character Data

-- | Read a character from JSON.  Return Maybe Character
readCharacter :: String -- ^ Filename
              -> IO (Maybe Character)
readCharacter fn = LB.readFile fn 
            >>= return . prepMaybe . decode
   where prepMaybe Nothing = Nothing
         prepMaybe (Just x) = Just $ prepareCharacter x


-- |
-- = Write Character Sheets

-- | Write a charactersheet in MarkDown, with both ingame and pregame logs.
-- This is currently not used.
writeCharacter :: String   -- ^ Directory for the output files
              -> SpellDB   -- ^ Spell database
              -> Character -- ^ Character whose sheet is written
              -> IO ()
writeCharacter dir db c = do
     writeOList fn $ printMDaug db c
     return ()
     where fn = dir ++ "/" ++ charID c ++ ".md"

-- | Write charactersheets for a list of characters
-- All advancement logs are written, both pregame and ingame.
-- This is currently not used.
writeCharacterList :: String -> SpellDB -> [Character] -> IO ()
writeCharacterList dir db cs = mapM (writeCharacter dir db) cs >> return ()

-- | Write charactersheets at Game Start in MarkDown
-- File name is derived from the character name.
writeGameStart :: String  -- ^ Directory for the output files
               -> Saga    -- ^ Saga whose characters are written
               -> IO ()
writeGameStart dir saga = mapM wf  cs >> return ()
     where db = spells saga
           cs = gameStartCharacters saga
           wf c = (writeOList (fn c) $ gameStartSheet db c)
           fn c = dir ++ "/" ++ charID c ++ ".md"

-- | Write current charactersheets in MarkDown
-- File name is derived from the character name.
writeCurrent :: String  -- ^ Directory for the output files
             -> Saga    -- ^ Saga whose characters are written
             -> IO ()
writeCurrent dir saga = mapM wf  cs >> return ()
     where db = spells saga
           cs = currentCharacters saga
           wf c = (writeOList (fn c) $ currentSheet db c)
           fn c = dir ++ "/" ++ charID c ++ ".md"

-- | Write current charactersheets in Long Format (MarkDown)
-- File name is derived from the character name.
writeLong :: String  -- ^ Directory for the output files
             -> Saga    -- ^ Saga whose characters are written
             -> IO ()
writeLong dir saga = mapM wf  cs >> return ()
     where db = spells saga
           cs = currentCharacters saga
           wf c = (writeOList (fn c) $ printSheetMD db c)
           fn c = dir ++ "/" ++ charID c ++ ".md"

-- |
-- = Read resources from CSV

-- | Read spells from CSV.  Return Maybe SpellDB.
readSpellDB :: String -- ^ Filename
              -> IO (Maybe SpellDB)
readSpellDB fn = parseFromFile CSV.csvFile fn >>= return . Just . spellDB . g
  where g (Left _) = [[]]
        g (Right x) = x
