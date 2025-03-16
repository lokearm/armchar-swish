{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.LongSheet
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Make a verbose character sheet in MarkDown.
--
-- The core of this module is the `Markdown` class and its `printMD`
-- function which renders an object in Markdown.
--
-----------------------------------------------------------------------------
module ArM.Char.LongSheet ( LongSheet(..) ) where

import Data.Maybe 
import Data.List 
import qualified Data.Map as M

import ArM.Char.Internal.Character 
import ArM.Char.CharacterSheet
import ArM.Char.Trait
import ArM.Char.Spell
import ArM.Char.Markdown
import ArM.GameRules
import ArM.BasicIO

-- import ArM.Debug.Trace

class Markdown a => LongSheet a where
   printSheetMD :: SpellDB
                -> a       -- ^ object to render
                -> OList   -- ^ list of lines for output
   printSheetMD = printMDaug

instance LongSheet CharacterSheet where
   printSheetMD db c' = OList 
               [ briefTraits c
               , showlistMD "+ **Characteristics:** "  $ charList c
               , showlistMD "+ **Personality Traits:** "  $ ptList c
               , showlistMD "+ **Reputations:** "  $ reputationList c
               , showlistMD "+ **Virtues and Flaws:** "  $ vfList c
               , showlistMD "+ **Abilities:** "  $ abilityList c
               , artMD c
               , printGrimoire $ spellList c
               , toOList $ printCastingTotals c
               ]
         where c = addCastingScores db c'

printGrimoire :: [Spell] -> OList
printGrimoire _ = OList $ OString "## Grimoire":OString "":xs 
    where xs = []


