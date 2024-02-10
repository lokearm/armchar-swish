{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.CharGen
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
import Data.Maybe (fromJust,catMaybes)
import Swish.RDF


printAdvancementLog :: [Advancement] -> [String]
printAdvancementLog = intercalate [] . map printAdvancement . filterAdv

advYear :: Advancement -> Maybe Int
advYear = charYear . advTime 

filterAdv :: [Advancement] -> [Advancement]
filterAdv []  = []
filterAdv (x:xs) | advYear x == Nothing = filterAdv xs 
filterAdv (x:xs) | otherwise = x:filterAdv xs 

printAdvancement :: Advancement -> [String]
printAdvancement = printAdvancement' 
printAdvancement' :: Advancement -> [String]
printAdvancement' ad = catMaybes 
                       [ Just $ "+ " ++ showSeason ad ++ ": " ++ fm (advType ad)
                       , f $ advLabel ad
                       , fi $ advXP ad
                       , f $ advDescription ad
                       , Just "    + Traits advanced"
                       ] ++
                       ( map ("        + "++) . map printTrait . traits ) ad
   where f Nothing = Nothing
         f (Just x) = Just $  "    + " ++ x
         fi Nothing = Nothing
         fi (Just x) = Just $ "    + Source Quality " ++ show x
         fm Nothing = ""
         fm (Just x) = x

showSeason :: Advancement -> String 
showSeason a =  ( charSeason . advTime ) a ++ " " ++ ( show . fromJust . advYear ) a 

data PTrait = PTrait 
     { ptXP :: Maybe Int
     , label1 :: Maybe String
     , label2 :: Maybe String
     , ptDetail :: Maybe String
     }
defaultPTrait :: PTrait 
defaultPTrait = PTrait 
     { ptXP = Nothing
     , label1 = Nothing
     , label2 = Nothing
     , ptDetail = Nothing
     }


ptLabel :: PTrait -> String
ptLabel pt = f (label1 pt) (label2 pt) ++ pShow (ptDetail pt)
   where f Nothing Nothing = "???"
         f Nothing (Just x) = x
         f (Just x) y = x ++ pShow y

makePTrait :: [RDFTriple] -> PTrait
makePTrait xs = makePTrait' xs defaultPTrait 

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
