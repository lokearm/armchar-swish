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
module ArM.Char.Markdown (printMD) where

-- import Data.Maybe (fromJust)
import Data.Maybe 
import ArM.Char.Character 
import ArM.Char.CharacterSheet
import ArM.Char.Trait
import ArM.Char.Advancement
import ArM.Debug.Trace

class Markdown a where
     printMD :: a -> [ String ]

instance Markdown FieldValue where
   printMD =  (:[]) . show

instance Markdown KeyPair where
   printMD (KeyPair x  y) = [ x, ':':' ':show y, "" ]
instance Markdown KeyPairList where
   printMD (KeyPairList xs) = ( foldl (++) [] $ map printMD xs )
instance Markdown CharacterConcept where
   printMD c = ("# " ++ fullConceptName c ):"": 
      ( printMD $ charGlance c ) ++ ( printMD $ charData c )

listMD :: Markdown a => [a] -> [String]
listMD = foldl (++) [] . map printMD 

pListMD :: Markdown a => String -> [a] -> [String]
pListMD _ [] = []
pListMD s x = ("":s:"":listMD x)

instance Markdown CharacterSheet where
   printMD c = ol ++ cl ++ ml -- foldl (:) ml cl
    where f _ [] = ""
          f s xs = foldl (++) s (map (++", ") $ map show xs)
          ml = [ "+ **Age:**"
               , f "+ **Characteristics:** "  $ charList c
               , f "+ **Personality Traits:** "  $ ptList c
               , f "+ **Reputations:** "  $ reputationList c
               , f "+ **Virtues and Flaws:** "  $ vfList c
               , f "+ **Abilities:** "  $ abilityList c
               , f "+ **Arts:** "  $ artList c
               , f "+ **Spells:** "  $ spellList c
               ]
          cl = foldl (++) [] $ map printMD $ confList c
          ol = foldl (++) [] $ map printMD $ csTraits c

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

instance Markdown CharacterState where
   printMD c = ( "## " ++ (show $ charTime c) ):"":sh
   -- pt c
       -- where pt = map ("+ "++) . foldl (++) [] . map printMD . traits 
       where sh = printMD $ filterCS c

showSQ :: Maybe Int -> Maybe Int -> String
showSQ Nothing Nothing = ""
showSQ (Just x) Nothing = "(" ++ show x ++ "xp)"
showSQ Nothing (Just x) = "(" ++ show x ++ "xp)"
showSQ (Just x) (Just y) = "(" ++ show x ++ "+" ++ show (y-x) ++ "xp)"

instance Markdown AugmentedAdvancement where
   printMD a = trace (show (vs a)) $ showTime xps (season a) (mode a) $ fn (narrative a) $ pt a
      where xps = showSQ (sourceQuality a) (effectiveSQ a)
            cs = map ("    + "++) . foldl (++) [] . map printMD . changes 
            vs = map ("    + "++) . map show . validation
            pt x = (cs x) ++ (vs x)
            fn Nothing xs = xs
            fn (Just x) xs = ( "    + " ++ show ( x ) ) :xs
showTime :: String -> SeasonTime -> Maybe String -> [String] -> [String]
showTime xps NoTime Nothing xs = ("+ ?? " ++ xps):xs
showTime xps  x Nothing xs = ("+ " ++ show x ++ xps):xs
showTime xps NoTime (Just x) xs = ("+ " ++ x ++ xps):xs
showTime xps x (Just y) xs = ("+ " ++ show x ++ xps ++ " " ++ y):xs
instance Markdown Advancement where
   printMD a = showTime xps (season a) (mode a) $ fn (narrative a) $ pt a
      where xps | sx == Nothing = ""
                | otherwise = " (" ++ ishow  sx ++ "xp)" 
            sx = sourceQuality a
            ishow = show . fromJust
            pt = map ("    + "++) . foldl (++) [] . map printMD . changes 

            fn Nothing xs = xs
            fn (Just x) xs = ( "    + " ++ show ( x ) ) :xs
instance Markdown Trait where
   printMD c = [ show c ]
instance Markdown ProtoTrait where
   printMD c = [ show c ]
