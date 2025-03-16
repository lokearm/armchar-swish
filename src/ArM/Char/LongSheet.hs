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
module ArM.Char.LongSheet ( printLongSheet ) where

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
   printSheetMD :: a       -- ^ object to render
                -> OList   -- ^ list of lines for output
   printSheetMD = printMD

advancementMD :: Character -> OList
advancementMD c = OList [ (pListMD "## Past Advancement" bs)
                        , (pListMD "## Future Advancement" cs) ]
mainstatMD :: Character -> OList
mainstatMD c = OList [ "+ **Characteristics:** "  $ charList c
               , f "+ **Personality Traits:** "  $ ptList c
               , f "+ **Reputations:** "  $ reputationList c ]
    where f _ [] = ""
          f s xs = foldl (++) s (map (++", ") $ map show xs)
printLongSheet :: SpellDB -> Character -> OList
printLongSheet db c
    | isNothing st = ( printMD . concept ) c 
    | otherwise = OList [ ( printMD . concept ) c 
                        , printSheet sheet
			, advancementMD c
			]
       where bs = pastAdvancement c
             cs = futureAdvancement c
             st = state c
             sheet = addCastingScores db $ filterCS $ fromJust st 

printSheet :: CharacterSheet -> OList
printSheet c = (cl ++ ml ++ lt ++ artl ++ ("":sl) ) 
    where f _ [] = ""
          f s xs = foldl (++) s (map (++", ") $ map show xs)
          ml = [ mainlistMD c
               , f "+ **Virtues and Flaws:** "  $ vfList c
               , f "+ **Abilities:** "  $ abilityList c
               , ""
               ]
          artl = artMD c
          lt = printCastingTotals c
          cl = briefTraits c
          sl = printGrimoire $ spellList c

printGrimoire :: [Spell] -> OList
printGrimoire _ = OString "## Grimoire":OString "":xs
    where xs = OList[]
