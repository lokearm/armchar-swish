{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Markdown
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Classes and instances to make MarkDown output.
--
-- The core of this module is the `Markdown` class and its `printMD`
-- function which renders an object in Markdown.
--
-----------------------------------------------------------------------------
module ArM.Char.Markdown ( Markdown(..)
                         , LongSheet(..)
                         , gameStartSheet
                         , currentSheet
                         ) where

import Data.Maybe 
import Data.List 
import qualified Data.Map as M

import ArM.Char.Internal.Character 
import ArM.Char.CharacterSheet
import ArM.Char.Trait
import ArM.Char.Advancement
import ArM.Char.Spell
import ArM.GameRules
import ArM.BasicIO

import ArM.Debug.Trace

-- |
-- = Rendering the Character Sheet

-- | Render a character sheet without advancement log
baseSheet :: SpellDB -> Character -> OList
baseSheet db c | isNothing (state c ) = s1
               | otherwise = OList [ s1, s2 ]
       where 
            s1 = printMD $ concept  c 
            s2 = printMDaug db $ state  c 

-- | Render a character sheet at game start.
-- Unlike the regular `printMD`, this includes only the character design
-- and not ingame advancements.
gameStartSheet :: SpellDB -> Character -> OList
gameStartSheet db c = OList
            [ baseSheet db c
            , OString ""
            , designMD c
            ]

-- | Render the current character sheet without pregame design details.
currentSheet :: SpellDB -> Character -> OList
currentSheet db c = OList [ baseSheet db c, advancementMD c ]

-- | Render the char gen design.
-- This is a list of all the pregame advancement objects.
designMD :: Character -> OList
designMD c = OList
            [ OString "## Game start design"
            , OString ""
            , OList $ map printMD $ pregameDesign c
            , OString ""
            ]

-- | Render the advancement log.
-- This is two lists of past and future advancement objects
advancementMD :: Character -> OList
advancementMD c = OList
            [ OString "## Past Advancement"
            , OString ""
            , OList $ map printMD $ pastAdvancement c
            , OString ""
            , OString "## Future Advancement" 
            , OString ""
            , OList $ map printMD $ futureAdvancement c
            , OString ""
            ]


-- |
-- = The Markdown Class

-- | Class defining `printMD` to render in Markdown.
class Markdown a where

     -- | This is the basic function to render in Markdown
     printMD :: a           -- ^ object to render
             -> OList       -- ^ list of lines for output

     -- | This is a hack to augment characters using extra resources
     -- By default, it is identical to `printMD`.
     printMDaug :: SpellDB    -- ^ Database of Spell information
                -> a          -- ^ object to render
                -> OList      -- ^ list of lines for output
     printMDaug _ = printMD

instance Markdown FieldValue where
   printMD  = OString . show

instance Markdown KeyPair where
   printMD (KeyPair x  y) = OList
         [ OString x
         , OString $ ':':' ':show y
         , OString "" ]
instance Markdown KeyPairList where
   printMD (KeyPairList xs) = OList $ map printMD xs
instance Markdown Trait where
   printMD = OString . show 
instance Markdown ProtoTrait where
   printMD = OString . show 

instance Markdown a => Markdown (Maybe a) where
   printMD Nothing = OList []
   printMD (Just x) = printMD x
   printMDaug _ Nothing = OList []
   printMDaug db (Just x) = printMDaug db x

-- | Render a list of objects as a comma-separated list on a single
-- line/paragraph.  This works for any instance of `Show`.
showlistMD :: Show a => String -> [a] -> OList
showlistMD _ [] = OList []
showlistMD s xs = OList [ OString s
                        , toOList $ (map (++", ") $ map show xs)
                        ]

-- | Render a Maybe String as an OList.
-- Nothing becomes an empty OList and a Just object becomes a single line.
stringMD :: Maybe String -> OList
stringMD Nothing = OList []
stringMD (Just x) = OString x

-- |
-- Markdown for the Character types

instance Markdown CharacterConcept where
   printMD c = OList
               [ OString ("# " ++ fullConceptName c )
               , OString ""
               , OString $ show (charType c)
               , OString $ ": " ++ ( fromMaybe "-" $ briefConcept c )
               , OString ""
               , OString "Quirk"
               , OString $ ": " ++ ( fromMaybe "-" $ quirk c )
               , OString ""
               , OString "Appearance" 
               , OString $ ": " ++ ( fromMaybe "-" $ appearance c )
               , OString ""
               , OString "Born" 
               , OString brn
               , OString ""
               , OString "Player" 
               , OString $ ": " ++ ( fromMaybe "-" $ player c )
               , OString ""
               , ( printMD $ charGlance c ) 
               , ( printMD $ charData c )
               ]
          where brn | born c == Nothing = ": ??" 
                    | otherwise = ": " ++ (show $ fromJust $ born c)



instance Markdown CharacterSheet where
   printMD c = OList 
               [ briefTraits c
               , showlistMD "+ **Characteristics:** "  $ charList c
               , showlistMD "+ **Personality Traits:** "  $ ptList c
               , showlistMD "+ **Reputations:** "  $ reputationList c
               , showlistMD "+ **Virtues and Flaws:** "  $ vfList c
               , showlistMD "+ **Abilities:** "  $ abilityList c
               , showlistMD "+ **Arts:** "  $ sortArts $ artList c
               , showlistMD "+ **Spells:** "  $ spellList c
               , toOList $ printCastingTotals c
               ]
   printMDaug db = printMD . addCastingScores db

instance Markdown CharacterState where
   printMD c = OList
       [ OString $ "## " ++ (show $ charTime c )
       , OString ""
       , printMD $ filterCS c
       ]
   printMDaug db c = OList
       [ OString $ "## " ++ (show $ charTime c) 
       , OString ""
       , printMDaug db $ filterCS c
       ]

-- |
-- == Markdown for Certain Traits

briefTraits :: CharacterSheet -> OList
briefTraits c = OList
          [ OString $ "+ **Age:** " ++ show (csAge c)
          , OList $ map printMD $ confList c
          , OList $ map printMD $ csTraits c
          ]
printCastingTotals :: CharacterSheet -> [String]
printCastingTotals c 
             | Magus /= csType c = []
             | otherwise = "":"| Casting Total | Creo | Intellego | Muto | Perdo | Rego |":
                              "|         :-    |  -:  |  -:       |  -:  |  -:   |  -:  |":
                              lts
      where
          lts = [ "| " ++ fo ++ foldl (++) "" (map ( (" | "++) . show ) ts ) ++ " |" 
                | (fo,ts) <- zip lforms (labTotals c) ]
          lforms = [ "Animal", "Aquam", "Auram", "Corpus", "Herbam", "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]

instance Markdown Confidence where
   printMD c = OString $
             "+ **" ++ cname c ++ "**: " ++ show (cscore c) ++ " ("
             ++ show (cpoints c) ++ ")" 
instance Markdown OtherTrait where
   printMD c = OString $
             "+ **" ++ trait c ++ "**: " ++ show (otherScore c) ++ " ("
             ++ show (otherExcess c) ++ ")" 

 
instance Markdown Character where
   printMD  c = OList
            [ bs 
            , designMD c
            , OString "## Pregame Development" 
            , OString ""
            , OList $ map printMD $ pregameAdvancement c
            , OString ""
            , advancementMD c
            ]
       where 
            bs | isNothing (state c ) = s1
               | otherwise = OList [ s1, s2 ]
            s1 = printMD $ concept  c 
            s2 = printMD $ state  c 
   printMDaug db c = OList
            [ baseSheet db c
            , designMD c
            , OString "## Pregame Development" 
            , OString ""
            , OList $ map printMD $ pregameAdvancement c
            , OString ""
            , advancementMD c
            ]

-- |
-- == Pretty print arts

-- | Render art scores as a table
artMD :: CharacterSheet
      -> OList
artMD = toOList . artMD'

-- | Render art scores as a table
artMD' :: CharacterSheet
      -> [ String ]
artMD' = ("":) . (h1:) . (h2:) . map artLine . sortArts . artList 
   where h1 = "| Art  | Score | XP |" 
         h2 = "| -: | -: | -: |"

-- | List of arts defined in *Ars Magica*
arts :: [ String ]
arts = [ "Creo", "Intellego", "Muto", "Perdo", "Rego",
         "Animal", "Aquam", "Auram", "Corpus", "Herbam", 
         "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]

-- | Map assigning sort index to each Art
sMap :: M.Map String Int 
sMap = M.fromList $ zip arts [1..15]

-- | Sort a list of arts in canonical order
sortArts :: [Art] -> [Art]
sortArts = sortOn ( fromMaybe 0 . ( \ x -> M.lookup x sMap ) . artName)

-- | Auxiliary for `artMD`, rendering a single line in the table
artLine :: Art -> String
artLine ar = "| " ++ artName ar  ++ " | " ++ show (artScore ar) ++ " | " ++ show (artExcessXP ar) ++ " |"

-- |
-- = Advancements

-- | Render the source quality of an advancement
showSQ :: Maybe XPType -> Maybe XPType -> String
showSQ Nothing Nothing = ""
showSQ (Just x) Nothing = " (" ++ show x ++ "xp)"
showSQ Nothing (Just x) = " (" ++ show x ++ "xp)"
showSQ (Just x) (Just y) = " (" ++ show x ++ f (y-x) ++ "xp)"
    where f 0 = ""
          f z = "+" ++ show z

instance Markdown AugmentedAdvancement where
   printMD a = indentOList $ OList
       [ OString $ showTime xps (season a) (mode a) y 
       , OList [ stringMD $ narrative a ]
       , OList $ map printMD $ changes a
       , OList $ map (OString . show) $ validation a
       ]
      where xps = showSQ (sourceQuality a) (effectiveSQ a)
            y = augYears a


-- | Render the season and mode of an advancement
showTime :: String -> SeasonTime -> Maybe String -> Maybe Int -> String
showTime xps NoTime Nothing y = ("?? " ++ xps ++ showYears y)
showTime xps  x Nothing y = (show x ++ xps ++ showYears y)
showTime xps NoTime (Just x) y = (x ++ xps ++ showYears y)
showTime xps x (Just z) y = (show x ++ xps ++ showYears y ++ " " ++ z)

-- | Render the duration of an advancement
showYears :: Maybe Int -> String
showYears Nothing = ""
showYears (Just x) = " (" ++ show x ++ " years)"

instance Markdown Advancement where
   printMD a = indentOList $ OList
         [ OString $ showTime xps (season a) (mode a) y 
         , OList [ stringMD $ narrative a ]
         , OList $ map printMD $ changes a
         ]
      where xps | sx == Nothing = ""
                | otherwise = " (" ++ ishow sx ++ "xp)" 
            sx = sourceQuality a
            ishow = show . fromJust
            y = advYears a

-- |
-- = Long Sheet Format


class Markdown a => LongSheet a where
   printSheetMD :: SpellDB
                -> a       -- ^ object to render
                -> OList   -- ^ list of lines for output
   printSheetMD = printMDaug

instance LongSheet Character where
   printSheetMD db c = trace "printSheetMD Character" $ OList 
            [ printMD $ concept c
            , sf 
            , designMD c
            , OString "## Pregame Development" 
            , OString ""
            , OList $ map printMD $ pregameAdvancement c
            , OString ""
            , advancementMD c
            ]
        where sf | isNothing (state c) = OList []
                 | otherwise = printSheetMD db $ characterSheet c

instance LongSheet CharacterSheet where
   printSheetMD db c' = trace "printSheetMD CharacterSheet" $ OList 
               [ briefTraits c
               , showlistMD "+ **Characteristics:** "  $ charList c
               , showlistMD "+ **Personality Traits:** "  $ ptList c
               , showlistMD "+ **Reputations:** "  $ reputationList c
               , showlistMD "+ **Virtues and Flaws:** "  $ vfList c
               , indentOList $ OList $ [ OString "**Abilities:**"
                        , OList (map (OString . show) ( abilityList c )) ]
               , artMD c
               , printGrimoire $ spellList c
               , toOList $ printCastingTotals c
               ]
         where c = addCastingScores db c'


-- | Render a spell trait in Markdown
-- The result should normally be subject to indentOList to make an hierarchical
-- list.
spellMD :: Spell -> OList
spellMD s = OList [ OString $ show s
                  , OList
                    [ masteryMD s
                    , f $ spellTComment s
                    ]
                  ]
     where f "" = OList [] 
           f x = OString x
masteryMD :: Spell -> OList
masteryMD s | 0 == masteryScore s && 0 == spellExcessXP s = OList []
            | otherwise = OString
                          $ "Mastery: " ++ show (masteryScore s)
                          ++ " (" ++ show (spellExcessXP s) ++ "xp) "
                          ++ show (masteryOptions s)

printGrimoire :: [Spell] -> OList
printGrimoire xs = OList [ OString "## Grimoire"
                         , OString ""
                         , OList $ map (indentOList . spellMD) xs ]


