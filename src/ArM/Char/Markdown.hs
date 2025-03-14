{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Char.Markdown (printMDaug,printMD,artMD) where

import Data.Maybe 
import Data.List 
import qualified Data.Map as M

import ArM.Char.Internal.Character 
import ArM.Char.CharacterSheet
import ArM.Char.Trait
import ArM.Char.Advancement
import ArM.Char.Spell

import ArM.Debug.Trace

class Markdown a where
     printMD :: a -> [ String ]
     printMDaug :: SpellDB -> a -> [ String ]
     printMDaug _ = printMD

instance Markdown FieldValue where
   printMD =  (:[]) . show

instance Markdown KeyPair where
   printMD (KeyPair x  y) = [ x, ':':' ':show y, "" ]
instance Markdown KeyPairList where
   printMD (KeyPairList xs) = ( foldl (++) [] $ map printMD xs )
instance Markdown CharacterConcept where
   printMD c = ("# " ++ fullConceptName c ):""
      : typ : con : ""
      : "Quirk" : qrk : ""
      : "Appearance" : app : ""
      : "Born" : brn : ""
      : "Player" : ply : ""
      : ( printMD $ charGlance c ) ++ ( printMD $ charData c )
          where typ = show (charType c)
                con = ": " ++ ( fromMaybe "-" $ briefConcept c )
                qrk = ": " ++ ( fromMaybe "-" $ quirk c )
                app = ": " ++ ( fromMaybe "-" $ appearance c )
                ply = ": " ++ ( fromMaybe "-" $ player c )
                brn | born c == Nothing = ": ??" 
                    | otherwise = ": " ++ (show $ fromJust $ born c)

listMD :: Markdown a => [a] -> [String]
listMD = foldl (++) [] . map printMD 

pListMD :: Markdown a => String -> [a] -> [String]
pListMD _ [] = []
pListMD s x = ("":s:"":listMD x)

artMD :: CharacterSheet -> [ String ]
artMD = ("":) . (h1:) . (h2:) . map artLine . sortArts . artList 
   where h1 = "| Art  | Score | XP |" 
         h2 = "| -: | -: | -: |"

arts :: [ String ]
arts = [ "Creo", "Intellego", "Muto", "Perdo", "Rego",
         "Animal", "Aquam", "Auram", "Corpus", "Herbam", 
         "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]
sMap :: M.Map String Int 
sMap = M.fromList $ zip arts [1..15]
sortArts :: [Art] -> [Art]
sortArts = sortOn ( fromMaybe 0 . ( \ x -> M.lookup x sMap ) . artName)

artLine :: Art -> String
artLine ar = "| " ++ artName ar  ++ " | " ++ show (artScore ar) ++ " | " ++ show (artExcessXP ar) ++ " |"

instance Markdown CharacterSheet where
   printMD c = ag:(ol ++ cl ++ ml ++ lt ) -- foldl (:) ml cl
    where f _ [] = ""
          f s xs = foldl (++) s (map (++", ") $ map show xs)
          ml = [ f "+ **Characteristics:** "  $ charList c
               , f "+ **Personality Traits:** "  $ ptList c
               , f "+ **Reputations:** "  $ reputationList c
               , f "+ **Virtues and Flaws:** "  $ vfList c
               , f "+ **Abilities:** "  $ abilityList c
               , f "+ **Arts:** "  $ sortArts $ artList c
               , f "+ **Spells:** "  $ spellList c
               ]
          -- artl = artMD c
          cl = foldl (++) [] $ map printMD $ confList c
          ol = foldl (++) [] $ map printMD $ csTraits c
          ag = "+ **Age:** " ++ show (csAge c)
          lt | Magus /= csType c = []
             | otherwise = "":"| Casting Total | Creo | Intellego | Muto | Perdo | Rego |":
                              "|         :-    |  -:  |  -:       |  -:  |  -:   |  -:  |":
                              lts
          lts = [ "| " ++ fo ++ foldl (++) "" (map ( (" | "++) . show ) ts ) ++ " |" 
                | (fo,ts) <- zip lforms (labTotals c) ]
          lforms = [ "Aninal", "Aquam", "Auram", "Corpus", "Herbam", "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]
   printMDaug db = printMD . addCastingScores db

instance Markdown Confidence where
   printMD c = [ "+ **" ++ cname c ++ "**: " ++ show (cscore c) ++ " ("
             ++ show (cpoints c) ++ ")" ]
instance Markdown OtherTrait where
   printMD c = [ "+ **" ++ trait c ++ "**: " ++ show (otherScore c) ++ " ("
             ++ show (otherExcess c) ++ ")" ]
 
instance Markdown Character where
   printMD c = ( printMD . concept ) c 
            ++ maybeP (state c)
            ++ (pListMD "## Game start design" as')
            ++ (pListMD "## Pregame Development" as)
            ++ (pListMD "## Past Advancement" bs)
            ++ (pListMD "## Future Advancement" cs)
       where 
             as' = pregameDesign c
             as = pregameAdvancement c
             bs = pastAdvancement c
             cs = futureAdvancement c
             maybeP  Nothing = []
             maybeP (Just xs) = printMD xs
   printMDaug db c = ( printMD . concept ) c 
            ++ maybeP (state c)
            ++ (pListMD "## Game start design" as')
            ++ (pListMD "## Pregame Development" as)
            ++ (pListMD "## Past Advancement" bs)
            ++ (pListMD "## Future Advancement" cs)
       where 
             as' = pregameDesign c
             as = pregameAdvancement c
             bs = pastAdvancement c
             cs = futureAdvancement c
             maybeP  Nothing = []
             maybeP (Just xs) = printMDaug db xs

instance Markdown CharacterState where
   printMD c = ( "## " ++ (show $ charTime c) ):"":sh
       where sh = printMD $ filterCS c
   printMDaug db c = ( "## " ++ (show $ charTime c) ):"":sh
       where sh = printMDaug db $ filterCS c

showSQ :: Maybe Int -> Maybe Int -> String
showSQ Nothing Nothing = ""
showSQ (Just x) Nothing = " (" ++ show x ++ "xp)"
showSQ Nothing (Just x) = " (" ++ show x ++ "xp)"
showSQ (Just x) (Just y) = " (" ++ show x ++ f (y-x) ++ "xp)"
    where f 0 = ""
          f z = "+" ++ show z

instance Markdown AugmentedAdvancement where
   printMD a = showTime xps (season a) (mode a) y : (fn (narrative a) $ pt a)
      where xps = showSQ (sourceQuality a) (effectiveSQ a)
            cs = map ("    + "++) . foldl (++) [] . map printMD . changes 
            vs = map ("    + "++) . map show . validation
            pt x = (cs x) ++ (vs x)
            fn Nothing xs = xs
            fn (Just x) xs = ( "    + " ++ show ( x ) ) :xs
            y = augYears a
showTime :: String -> SeasonTime -> Maybe String -> Maybe Int -> String
showTime xps NoTime Nothing y = ("+ ?? " ++ xps ++ showYears y)
showTime xps  x Nothing y = ("+ " ++ show x ++ xps ++ showYears y)
showTime xps NoTime (Just x) y = ("+ " ++ x ++ xps ++ showYears y)
showTime xps x (Just z) y = ("+ " ++ show x ++ xps ++ showYears y ++ " " ++ z)
showYears :: Maybe Int -> String
showYears Nothing = ""
showYears (Just x) = " (" ++ show x ++ " years)"

instance Markdown Advancement where
   printMD a = showTime xps (season a) (mode a) y : (fn (narrative a) $ pt a)
      where xps | sx == Nothing = ""
                | otherwise = " (" ++ ishow  sx ++ "xp)" 
            sx = sourceQuality a
            ishow = show . fromJust
            pt = map ("    + "++) . foldl (++) [] . map printMD . changes 

            fn Nothing xs = xs
            fn (Just x) xs = ( "    + " ++ show ( x ) ) :xs
            y = advYears a
instance Markdown Trait where
   printMD c = [ show c ]
instance Markdown ProtoTrait where
   printMD c = [ show c ]
