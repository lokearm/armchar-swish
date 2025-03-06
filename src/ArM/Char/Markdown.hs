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
import Data.Maybe (fromJust)
import ArM.Char.Character 
import ArM.Char.Trait
import ArM.Char.Advancement

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

instance Markdown Character where
   printMD c = ( printMD . concept ) c 
            ++ maybeP (state c)
            ++ ("":"## Pregame Development":"":listMD as)
            ++ ("":"## Past Advancement":"":listMD bs)
            ++ ("":"## Future Advancement":"":listMD cs)
       where 
             as = pregameAdvancement c
             bs = pastAdvancement c
             cs = futureAdvancement c
             maybeP  Nothing = []
             maybeP (Just xs) = printMD xs

instance Markdown CharacterState where
   printMD c = ( "## " ++ show (charTime c) ):"":pt c
       where pt = map ("+ "++) . foldl (++) [] . map printMD . traits 

showSQ :: Maybe Int -> Maybe Int -> String
showSQ Nothing Nothing = ""
showSQ (Just x) Nothing = "(" ++ show x ++ "xp)"
showSQ Nothing (Just x) = "(" ++ show x ++ "xp)"
showSQ (Just x) (Just y) = "(" ++ show x ++ "+" ++ show (y-x) ++ "xp)"

instance Markdown AugmentedAdvancement where
   printMD a = f (season a) (mode a) $ fn (narrative a) $ pt a
      where xps = showSQ (sourceQuality a) (effectiveSQ a)
            pt = map ("    + "++) . foldl (++) [] . map printMD . changes 

            fn Nothing xs = xs
            fn (Just x) xs = ( "    + " ++ show ( x ) ) :xs
            f Nothing Nothing xs = ("+ ?? " ++ xps):xs
            f (Just x) Nothing xs = ("+ " ++ x ++ xps):xs
            f Nothing (Just x) xs = ("+ " ++ x ++ xps):xs
            f (Just x) (Just y) xs = ("+ " ++ x ++ xps):("    + " ++ y):xs
instance Markdown Advancement where
   printMD a = f (season a) (mode a) $ fn (narrative a) $ pt a
      where xps | sx == Nothing = ""
                | otherwise = " (" ++ ishow  sx ++ "xp)" 
            sx = sourceQuality a
            ishow = show . fromJust
            pt = map ("    + "++) . foldl (++) [] . map printMD . changes 

            fn Nothing xs = xs
            fn (Just x) xs = ( "    + " ++ show ( x ) ) :xs
            f Nothing Nothing xs = ("+ ?? " ++ xps):xs
            f (Just x) Nothing xs = ("+ " ++ x ++ xps):xs
            f Nothing (Just x) xs = ("+ " ++ x ++ xps):xs
            f (Just x) (Just y) xs = ("+ " ++ x ++ xps):("    + " ++ y):xs
instance Markdown Trait where
   printMD c = [ show c ]
instance Markdown ProtoTrait where
   printMD c = [ show c ]
