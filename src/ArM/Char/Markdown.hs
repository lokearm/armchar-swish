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
-- function which renders an object in Markdown.  There is also 
-- a `LongSheet` class with a `printSheetMD` function for a more
-- verbose character sheet.
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

import ArM.Char.Character 
import ArM.Char.CharacterSheet
import ArM.Char.Trait
import ArM.Char.Spell
import ArM.GameRules
import ArM.BasicIO

-- import ArM.Debug.Trace

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
designMD c  | as == [] = OList []
            | otherwise = OList
            [ OString "## Game start design"
            , OString ""
            , OList $ map printMD as
            , OString ""
            ]
            where as = pregameDesign c

-- | Render the char gen design.
-- This is a list of all the pregame advancement objects.
chargenMD :: Character -> OList
chargenMD c | as == [] = OList []
            | otherwise = OList
              [ OString "## Char Gen Advancements"
              , OString ""
              , OList $ map printMD as
              , OString ""
              ]
            where as = pregameAdvancement c

-- | Render the advancement log.
-- This is two lists of past and future advancement objects
advancementMD :: Character -> OList
advancementMD c = OList [ ao, bo ]
   where as = pastAdvancement c
         bs = futureAdvancement c
         ao | as == [] = OList []
            | otherwise = OList
                [ OString "## Past Advancement"
                , OString ""
                , OList $ map printMD as
                , OString ""
                ]
         bo | bs == [] = OList []
            | otherwise = OList
                [ OString "## Future Advancement"
                , OString ""
                , OList $ map printMD bs
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

instance Markdown a => Markdown (Maybe a) where
   printMD Nothing = OList []
   printMD (Just x) = printMD x
   printMDaug _ Nothing = OList []
   printMDaug db (Just x) = printMDaug db x

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
   printMD (AgeTrait x) = printMD  x
   printMD x = OString $ show  x
instance Markdown ProtoTrait where
   printMD = OString . show 


-- | Render a list of objects as a comma-separated list on a single
-- line/paragraph.  This works for any instance of `Show`.
showlistMD :: Show a => String -> [a] -> OList
showlistMD _ [] = OList []
showlistMD s xs = OList [ OString s
                        , toOList $ (map (++", ") $ map show xs)
                        ]

-- | Render a Maybe String as an OList.
-- Nothing becomes an empty OList and a Just object becomes a single line.
-- Note that this is different from the generic instance for Maybe, because
-- of the difficulties making an instance for String.
stringMD :: Maybe String -> OList
stringMD Nothing = OList []
stringMD (Just x) = OString x

-- |
-- - Markdown for the Character types
-- 
-- The `CharacterConcept` is set as a description list.
-- 
-- This may cause problems with long text values.  It would be worth distinguishing
-- between more fields and use a differfent formatting where long text is expected.

instance Markdown Character where
   printMD  c = OList
            [ bs 
            , designMD c
            , chargenMD c
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
            , chargenMD c
            , advancementMD c
            ]


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
-- == Markdown for Age, Confidence, Warping, and Decreptitude

-- | Print age, confidence, warping, and decrepitude as bullt points
briefTraits :: CharacterSheet -> OList
briefTraits c = OList
          [ printMD (ageObject c)
          , OList $ map printMD $ confList c
          , OList $ map printMD $ otherList c
          -- , OList $ map printMD $ csTraits c
          ]

-- | Print a table of casting totals for every TeFo combination.
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

instance Markdown Age where
   printMD c = OString $ "+ **Age:** " ++ show y ++ " years (apparent age " 
            ++ show (y - apparentYounger c)  ++ ")" ++ lr
      where y = ageYears c
            lrs = longevityRitual c
            lr | lrs < 0 = ""
               | otherwise = " Longeevity Ritual: " ++ show lrs
instance Markdown Confidence where
   printMD c = OString $
             "+ **" ++ cname c ++ "**: " ++ show (cscore c) ++ " ("
             ++ show (cpoints c) ++ ")" 
instance Markdown OtherTrait where
   printMD c = OString $
             "+ **" ++ trait c ++ "**: " ++ show (otherScore c) ++ " ("
             ++ show (otherExcess c) ++ ")" 



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
       , infl
       , OList $ map (OString . show) $ validation a
       ]
      where xps = showSQ (sourceQuality a) (effectiveSQ a)
            y = augYears a
            inf = inferredTraits a
            infl | inf == [] = OList []
                 | otherwise = OList [ OString "Inferred traits", OList $ map printMD inf ]


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


-- | The `LongSheet` class is similar to `Markdown`, but is
-- intended to make a longer output with both more space and more
-- verbose text.  
--
-- In the current impplementation, it is only the character state
-- which is made longer, by setting abilities and spells as bullet
-- points instead of a single paragraph for the full list.
class Markdown a => LongSheet a where
   -- | By default `printSheetMD` is identical to `printMDaug`
   printSheetMD :: SpellDB -- ^ spell database
                -> a       -- ^ object to render
                -> OList   -- ^ list of lines for output
   printSheetMD = printMDaug

instance LongSheet Character where
   printSheetMD db c = OList 
            [ printMD $ concept c
            , sf $ state c 
            , designMD c
            , chargenMD c
            , advancementMD c
            ]
        where sf Nothing = OList []
              sf (Just s) = printSheetMD db s
instance LongSheet CharacterState where
   printSheetMD db c = OList [ OString $ "## " ++ (show $ charTime c )
                             , OString ""
                             , printSheetMD db $ filterCS c ]

instance LongSheet CharacterSheet where
   printSheetMD db c' = OList 
               [ briefTraits c
               , showlistMD "+ **Characteristics:** "  $ charList c
               , showlistMD "+ **Personality Traits:** "  $ ptList c
               , showlistMD "+ **Reputations:** "  $ reputationList c
               , showlistMD "+ **Virtues and Flaws:** "  $ vfList c
               , indentOList $ OList $ [ OString "**Abilities:**"
                        , OList (map (OString . show) ( abilityList c )) ]
               , mag
               ]
         where c = addCastingScores db c'
               mag | isMagus c' = OList [ artMD c, printFullGrimoire db $ spellList c, 
                                          toOList $ printCastingTotals c ]
                   | otherwise = OString "" 

-- |
-- == Pretty print arts

-- | Render art scores as a table
artMD :: CharacterSheet
      -> OList
artMD c | isMagus c = toOList $ artMD' c
        | otherwise = OList []

isMagus :: CharacterSheet -> Bool
isMagus c = csType c == Magus

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
-- == Render Spells


-- | Render a spell trait in Markdown
-- The result should normally be subject to indentOList to make an hierarchical
-- list.
spellDescMD :: (Spell,Maybe SpellRecord) -> OList
spellDescMD (s,sr) = OList [ OString $ show s
                  , OList [ masteryMD s, f $ spellTComment s ]
                  , coreSpellRecordMD sr
                  ]
     where f "" = OList [] 
           f x = OString x

-- | Set all information from mastery on one line.
-- This includes mastery score, xp, and mastery options.
masteryMD :: Spell -> OList
masteryMD s | 0 == masteryScore s && 0 == spellExcessXP s = OList []
            | otherwise = OString
                          $ "Mastery: " ++ show (masteryScore s)
                          ++ " (" ++ show (spellExcessXP s) ++ "xp) "
                          ++ show (masteryOptions s)


-- | Set a list of spells.
-- Each spell is set using `spellMD`, and the result is indented as a
-- hierarchical list.
printFullGrimoire :: SpellDB -> [Spell] -> OList
printFullGrimoire db xs = OList [ OString "## Grimoire"
                         , OString ""
                         , OList $ map (indentOList . spellDescMD) ys 
                         , OString ""
                         , OString $ "Total: " ++show (totalLevels xs)  
                            ++ " levels of spells."
                         ]
   where ys = [ (x,spellLookup (traitKey x) db ) | x <- xs ]


-- | Return the sum of levels in the list of spells.
totalLevels :: [Spell] -> Int
totalLevels = sum . map spellLevel

coreSpellRecordMD :: Maybe SpellRecord -> OList
coreSpellRecordMD Nothing = OList []
coreSpellRecordMD sr = OList [ reqstr
                             , OString $ (show $ rdt sp)
                               ++ (foldl (++) "" $ map (", "++) $  specialSpell sp)
                             , os (description sp)
                             , os (design sp)
                             , os (cite sp)
                    ]
   where req = techniqueReq sp ++ formReq sp
         sp = fromJust sr
         os "" = OList []
         os x = OString x
         reqstr | req == [] = OList []
                | otherwise = OString $ "Req. " ++ show req

{-
-- | Render a spell trait in Markdown
-- The result should normally be subject to indentOList to make an hierarchical
-- list.
spellMD :: Spell -> OList
spellMD s = OList [ OString $ show s
                  , OList [ masteryMD s, f $ spellTComment s ]
                  ]
     where f "" = OList [] 
           f x = OString x

-- | Set a list of spells.
-- Each spell is set using `spellMD`, and the result is indented as a
-- hierarchical list.
printGrimoire :: [Spell] -> OList
printGrimoire xs = OList [ OString "## Grimoire"
                         , OString ""
                         , OList $ map (indentOList . spellMD) xs 
                         , OString ""
                         , OString $ "Total: " ++show (totalLevels xs)  
                            ++ " levels of spells."
                         ]
-}
