
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Spell
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Spell Records
--
--
-----------------------------------------------------------------------------
module ArM.Char.Spell ( SpellRecord(..)
                       , spellDB
                       ) where

import ArM.Char.Trait
import GHC.Generics
import Data.List.Split
import qualified Data.Map as M

data SpellRecord = SpellRecord
                   { spellKey :: TraitKey   -- ^ Unique identifier as used in `ArM.Char.Trait`
                   , lvl :: Maybe Int       -- ^ Spell Level.  General Level Spells have Nothing.
                   , technique :: String
                   , techniqueReq :: [String]
                   , form :: String
                   , formReq :: [String]
                   , rdt :: (String,String,String)   -- ^ Range/Duration/Target
                   , specialSpell :: [String]        -- ^ Special tags, like Ritual or Mutantum.
                   , description :: String           -- ^ Freeform description of the effect
                   , design :: String                -- ^ Level calculation
                   , comment :: String               -- ^ Freeform remarks that do not fit elsewhere
                   , cite :: String                  -- ^ Source reference
                   }
           deriving (Ord, Eq, Generic)

-- | Default SpellRecord object as a starting point for step-by-step construction.
defaultSR :: SpellRecord
defaultSR = SpellRecord
                   { spellKey = OtherTraitKey "None"
                   , lvl = Nothing
                   , technique = ""
                   , techniqueReq = []
                   , form = ""
                   , formReq = []
                   , rdt = ("Per","Mom","Ind")
                   , specialSpell = []
                   , description = ""
                   , design = ""
                   , comment = ""
                   , cite = ""
                   }

-- | Create a `Data.Map.Map` of SpellRecord objects.  
-- The input is the output from `Data.CSV.csvFile`
spellDB :: [[String]] -> M.Map TraitKey SpellRecord
spellDB = M.fromList . map ( \ x -> (spellKey x,x) ) . map fromCSVline

-- | Parse the cells of one line from the CSV file into a SpellRecord object.
fromCSVline :: [String] -> SpellRecord
fromCSVline (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:_) =
      defaultSR { spellKey = SpellKey x1 x2
                , lvl = read x3
                , technique = x4
                , techniqueReq = splitOn ";" x5
                , form = x6
                , formReq = splitOn ";" x7
                , specialSpell = splitOn ";" x8
                , description = x9
                , design = x10
                , comment = x11
                , cite = x12
                }
fromCSVline _ = defaultSR
