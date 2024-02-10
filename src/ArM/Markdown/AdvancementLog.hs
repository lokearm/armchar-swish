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


import ArM.Types.Advancement
import ArM.Types.Season
import Data.List(intercalate)
import Data.Maybe (fromJust,catMaybes)


printAdvancementLog :: [Advancement] -> [String]
printAdvancementLog = intercalate [] . map printAdvancement . filterAdv

advYear :: Advancement -> Maybe Int
advYear = charYear . advTime 

filterAdv :: [Advancement] -> [Advancement]
filterAdv []  = []
filterAdv (x:xs) | advYear x == Nothing = filterAdv xs 
filterAdv (x:xs) | otherwise = x:filterAdv xs 

printAdvancement :: Advancement -> [String]
printAdvancement = catMaybes . printAdvancement' 
printAdvancement' :: Advancement -> [Maybe String]
printAdvancement' ad = [ Just $ "+ " ++ showSeason ad ++ ": " ++ fm (advType ad),
                         f $ advLabel ad,
                         f $ advDescription ad
                       ]
   where f Nothing = Nothing
         f (Just x) = Just $  "    + " ++ x
         fm Nothing = ""
         fm (Just x) = x

showSeason :: Advancement -> String 
showSeason a =  ( charSeason . advTime ) a ++ " " ++ ( show . fromJust . advYear ) a 

