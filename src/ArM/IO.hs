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
-- import qualified Data.CSV as CSV
import qualified Data.ByteString.Lazy as LB

-- import Text.ParserCombinators.Parsec
import System.Directory

import ArM.Char.Character
import ArM.Markdown
import ArM.Cov.Saga
import ArM.DB.CSV
import ArM.DB.Weapon()
import ArM.BasicIO
import ArM.Helper

import ArM.Debug.Trace

-- |
-- = Read Saga Files
-- | Read a saga from JSON.  Return Maybe SagaFile
readSagaFile :: String -- ^ Filename
             -> IO (Maybe SagaFile)
readSagaFile fn = LB.readFile fn >>= return . decode

-- | Load the saga and its constituent objects from the given file.
-- This calls both `readSagaFile` and `loadSaga`.
readSaga :: String -- ^ Filename
         -> IO (Maybe Saga)
readSaga fn = readSagaFile fn >>= passMaybe loadSaga


-- | Load constituent objects for a saga.
loadSaga :: SagaFile -> IO Saga
loadSaga saga = do
   db <- readDB $ spellFile saga
   wdb <- readDB $ weaponFile saga
   adb <- readDB $ armourFile saga
   cs <- mapM readCharacter $ characterFiles saga
   return
     $ advanceSaga saga
     $ Saga { rootDir = fromMaybe "/tmp/" $ rootDirectory saga
           , sagaStates = SagaState
              { stateTitle = title saga
              , covenants = []  
              , characters = filterNothing cs  
              , seasonTime = GameStart
              }:[]
           , gameStartCharacters = []
           , sagaTitle = title saga
           , spells = fromJust db 
           , weapons = fromJust wdb
           , armour = fromJust adb
           }

-- | -- Write write character sheets (long format) for the characters in the
-- given saga to the given directory.
-- File names are derived from character names.
longSheet :: String    -- ^ Directory name
          -> Saga      -- ^ Saga whose characters to write
          -> IO Saga
longSheet dir s = trace "Write longSheet" $ writeLong dir s >> return s


writeSaga :: Saga -> IO ()
writeSaga saga = do
   createDirectoryIfMissing True gsf
   createDirectoryIfMissing True cdir
   createDirectoryIfMissing True longDir
   writeGameStart gsf  saga
   writeCurrent cdir  saga
   writeOList (rootDir saga ++ "/index.md") $ sagaIndex saga
   writeOList (gsf ++ sagaStartName saga ++ ".md") $ sagaGameStartIndex saga
   writeOList (cdir ++ sagaStateName saga ++ ".md") $ sagaCurrentIndex saga
   writeLns (gsf ++ sagaStartName saga ++ " Errors.md" ) $
                  "# Errors in Character Design":"":pregameErrors saga
   writeLns (cdir ++ sagaStateName saga ++ " Errors.md" ) $
                  "# Errors in Advancement":"":ingameErrors saga
   _ <- longSheet longDir saga

   return () 
        where gsf = gamestartDir saga
              cdir = currentDir saga
              longDir = cdir ++ "/LongSheet/"



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
              -> Saga      -- ^ Saga containing databases for spells etc.
              -> Character -- ^ Character whose sheet is written
              -> IO ()
writeCharacter dir saga c = do
     writeOList fn $ printMDaug saga c
     return ()
     where fn = dir ++ "/" ++ charID c ++ ".md"

-- | Write charactersheets for a list of characters
-- All advancement logs are written, both pregame and ingame.
-- This is currently not used.
writeCharacterList :: String -> Saga -> [Character] -> IO ()
writeCharacterList dir saga cs = mapM (writeCharacter dir saga) cs >> return ()

-- | Write charactersheets at Game Start in MarkDown
-- File name is derived from the character name.
writeGameStart :: String  -- ^ Directory for the output files
               -> Saga    -- ^ Saga whose characters are written
               -> IO ()
writeGameStart dir saga = mapM wf  cs >> return ()
     where cs = gameStartCharacters saga
           wf c = (writeOList (fn c) $ gameStartSheet saga c)
           fn c = dir ++ "/" ++ characterStartName c ++ ".md"

-- | Write current charactersheets in MarkDown
-- File name is derived from the character name.
writeCurrent :: String  -- ^ Directory for the output files
             -> Saga    -- ^ Saga whose characters are written
             -> IO ()
writeCurrent dir saga = mapM wf  cs >> return ()
     where cs = currentCharacters saga
           wf c = (writeOList (fn c) $ currentSheet saga c)
           fn c = dir ++ "/" ++ characterStateName c ++ ".md"

-- | Write current charactersheets in Long Format (MarkDown)
-- File name is derived from the character name.
writeLong :: String  -- ^ Directory for the output files
             -> Saga    -- ^ Saga whose characters are written
             -> IO ()
writeLong dir saga = mapM wf  cs >> return ()
     where cs = currentCharacters saga
           wf c = (writeOList (fn c) $ printSheetMD saga c)
           fn c = dir ++ "/" ++ charID c ++ ".md"

