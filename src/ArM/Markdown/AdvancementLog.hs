{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.AdvancementLog
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle characters as stored in web server memory.
--
-----------------------------------------------------------------------------
module ArM.Markdown.AdvancementLog ( printAdvancementLog ) where


import ArM.Markdown.CharacterSheet
import ArM.Rules.Aux
import ArM.Resources
import ArM.Types.Trait
import ArM.Types.Advancement
import ArM.Types.Season
import Data.List(intercalate)
import Data.Maybe (catMaybes)
import Swish.RDF


printAdvancementLog :: [Advancement] -> [String]
printAdvancementLog = intercalate [] . map printAdvancement . filterAdv

-- advYear :: Advancement -> Maybe Int
-- advYear = charYear . advTime 

filterAdv :: [Advancement] -> [Advancement]
filterAdv = reverse
{-
filterAdv []  = []
filterAdv (x:xs) | advYear x == Nothing = filterAdv xs 
filterAdv (x:xs) | otherwise = x:filterAdv xs 
-}

showAdType :: Advancement -> String
showAdType ad | y ad == Nothing = "+ " ++ showSeason ad 
              | otherwise = "+ " ++ showSeason ad ++ ": " ++ fm (advType ad)
   where y = charYear . advTime
         fm Nothing = ""
         fm (Just x) = x

printAdvancement :: Advancement -> [String]
printAdvancement ad = catMaybes 
                       [ Just $ showAdType ad
                       , f $ advLabel ad
                       , fi $ advXP ad
                       , f $ advDescription ad
                       , xpCount (spentXP ad) (advXP ad)
                       , lvlCount (spellLevels ad) (advLevels ad)
                       , Just "    + Traits advanced"
                       ] ++
                       ( map ("        + "++) . map printTrait . traits ) ad
   where f Nothing = Nothing
         f (Just x) = Just $  "    + " ++ x
         fi Nothing = Nothing
         fi (Just x) = Just $ "    + Source Quality " ++ show x
lvlCount :: Maybe Int -> Maybe Int -> Maybe String
lvlCount Nothing Nothing = Nothing
lvlCount (Just 0) Nothing = Nothing
lvlCount (Just 0) (Just 0) = Nothing
lvlCount Nothing (Just y) = Just $
     "    +  " ++ show y ++ " spell levels to spend"
lvlCount (Just x) Nothing = Just $
     "    + Gained " ++ show x ++ " levels of spells"
lvlCount (Just x) (Just y) | x < y = Just $
     "    + **Underspent** " ++ show x ++ " spell levels of " ++ show y ++ " levels possible"
lvlCount (Just x) (Just y) | x > y = Just $
     "    + **Overspent** " ++ show x ++ " spell levels of " ++ show y ++ " levels possible"
lvlCount (Just x) (Just y) | otherwise = Just $
     "    + Gained " ++ show x ++ " spell levels (of an allowance of " ++ show y ++ " levels)"

xpCount :: Maybe Int -> Maybe Int -> Maybe String
xpCount Nothing Nothing = Nothing
xpCount (Just 0) Nothing = Nothing
xpCount Nothing (Just y) = Just $
     "    + **Underspent**: " ++ show y ++ "xp to spend"
xpCount (Just x) Nothing = Just $
     "    + **Overspent**: No allowance, but " ++ show x ++ "xp spent"
xpCount (Just x) (Just y) | x < y = Just $
     "    + **Underspent** " ++ show x ++ "xp of " ++ show y ++ "xp"
xpCount (Just x) (Just y) | x > y = Just $
     "    + **Overspent** " ++ show x ++ "xp of " ++ show y ++ "xp"
xpCount (Just x) (Just y) | otherwise = Just $
     "    + Spent " ++ show x ++ "xp of " ++ show y ++ "xp"

showSeason :: Advancement -> String 
showSeason =  show . advTime
-- showSeason a =  ( charSeason . advTime ) a ++ " " ++ ( show . fromJust . advYear ) a 

data PTrait = PTrait 
     { ptXP :: Maybe Int
     , ptID :: String
     , label1 :: Maybe String
     , label2 :: Maybe String
     , ptDetail :: Maybe String
     }
defaultPTrait :: PTrait 
defaultPTrait = PTrait 
     { ptXP = Nothing
     , ptID = ""
     , label1 = Nothing
     , label2 = Nothing
     , ptDetail = Nothing
     }


ptLabel :: PTrait -> String
ptLabel pt = f (label1 pt) (label2 pt) ++ pShow (ptDetail pt)
   where f Nothing Nothing = "??? " ++ ptID pt
         f Nothing (Just x) = x
         f (Just x) y = x ++ pShow y

makePTrait :: [RDFTriple] -> PTrait
makePTrait [] = defaultPTrait 
makePTrait xs = makePTrait' xs defaultPTrait { ptID = show $ arcSubj $ head xs }

makePTrait' :: [RDFTriple] -> PTrait -> PTrait
makePTrait' [] pt = pt
makePTrait' (x:xs) pt 
    | arcPred x == armRes "instanceLabel" = makePTrait' xs $ pt { label1 = rdfToString (arcObj x) }
    | arcPred x == armRes "hasLabel" = makePTrait' xs $ pt { label2 = rdfToString (arcObj x) }
    | arcPred x == armRes "addedXP" = makePTrait' xs $ pt { ptXP = rdfToInt (arcObj x) }
    | otherwise = makePTrait' xs $ pt

printTrait :: Trait -> String
printTrait = printTrait' . makePTrait . traitContents
printTrait' :: PTrait -> String
printTrait' t = ptLabel t ++ xp (ptXP t)
    where xp Nothing = ""
          xp (Just x) = " (" ++ show x ++ " xp)"
