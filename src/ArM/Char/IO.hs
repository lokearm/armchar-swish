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
import Data.Aeson (decode)
import qualified Data.CSV as CSV
-- import Data.Text.Lazy.IO as I
import qualified Data.ByteString.Lazy as LB

import Text.ParserCombinators.Parsec

import ArM.Char.Character
import ArM.Char.Spell

-- | Read a character from JSON.  Return Maybe Character
readCharacter :: String -- ^ Filename
              -> IO (Maybe Character)
readCharacter fn = LB.readFile fn 
            >>= return . prepMaybe . decode
   where prepMaybe Nothing = Nothing
         prepMaybe (Just x) = Just $ prepareCharacter x

-- | Read spells from CSV.  Return Maybe SpellDB.
readSpellDB :: String -- ^ Filename
              -> IO (Maybe SpellDB)
readSpellDB fn = parseFromFile CSV.csvFile fn >>= return . Just . spellDB . g
  where g (Left _) = [[]]
        g (Right x) = x
