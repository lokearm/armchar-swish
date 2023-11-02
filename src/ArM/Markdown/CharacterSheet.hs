{-# LANGUAGE OverloadedStrings #-}
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
module ArM.Markdown.CharacterSheet ( printSheetObject ) where


import ArM.Markdown.SheetObject
import Data.List(intercalate)

printSheetObject :: SheetObject -> [String]
printSheetObject ob = (map printMD $ metadata ob) 
                  ++ [ "Personality traits",
                       (f $ map p1 $ ptraits ob),
                       "",
                       "Characteristics",
                       (f $ map p2 $ characteristics ob),
                       "",
                       "Size",
                       ": " ++ (lf $ map maybeFormat $ size ob),
                       "",
                       "Confidence",
                       ": " ++ (lf $ map confFormat $ cnf ob),
                       "",
                       "# Virtues and Flaws",
                       ""
                       ]
                  ++ (map printVFLine $ virtues ob)
                  ++ (map printVFLine $ flaws ob)
                  ++ [ "", "# Abilities", "",
                       "| Ability\t | Speciality\t | Score\t| XP\t| Bonus\t| Effective\t|",
                       "| :-     \t | :-        \t |   -: \t| -:\t|   -: \t|       -: \t|"
                       ]
                  ++ (map printAbilityLine $ abilities ob)
                  ++ [ "", "# Arts", "",
                       "| Art\t | Score\t | XP\t| Bonus\t| Effective\t|",
                       "| :- \t |    -:\t | -:\t|   -: \t|       -: \t|"
                       ]
                  ++ (map printArtLine $ arts ob)
                  ++ [ "", "# Spells", "" ]
                  ++ (map printSpellLine $ spells ob)
   where p1 x = fJ (traitLabel x) ++ " " ++ (fI $ traitTotalScore x)
         fJ Nothing = "???"
         fJ (Just x) = x
         fI Nothing = "???"
         fI (Just x) = show x
         f = (':':) . (' ':) . intercalate ", "  
         p2 x = fJ (traitAbbr x) ++ " " ++ (fI $ traitTotalScore x)
         lf [] = "-"
         lf xs = intercalate ", " xs

printSpellLine :: Trait -> String
printSpellLine t = "+ " ++ tefoString t ++ " " ++ f1 t 
                        ++ f3 t ++ " *Casting Score* " ++ f2 t 
                        ++ "; *Mastery* "
      ++ f4 t ++ " (" ++ f5 t ++ ")"
   where vfDetail Nothing = "" 
         vfDetail (Just s) = " [" ++ s ++ "]"
         f3 = vfDetail . traitDetail
         f1 = ss . traitLabel
         f4 = si . traitScore
         f5 = si . traitXP
         f2 = si . traitCastingScore

printMD :: (String,String) -> String
printMD (x, y) = x ++ "\n: " ++ y ++ "\n"

confFormat :: (Maybe Int, Maybe Int) -> String
confFormat (x,y) = maybeFormat x ++ " (" ++ maybeFormat y ++ ")"
maybeFormat :: (Show a) => Maybe a -> String
maybeFormat Nothing = "-"
maybeFormat (Just x) = show x


printVFLine :: Trait -> String
printVFLine t = "+ " ++ f1 t ++ f3 t ++ " (" ++ f2 t ++ ")"
   where vfDetail Nothing = "" 
         vfDetail (Just s) = ": " ++ s
         f3 = vfDetail . traitDetail
         f1 = ss . traitLabel
         f2 = si . traitScore



printArtLine :: Trait -> String
printArtLine t = "| " ++ (ss $ traitLabel t) ++ "\t | " 
                         ++ (si $ traitScore t) ++ "\t| "
                         ++ (si $ traitXP  t) ++ "\t|"
                         ++ (si $ traitBonus  t) ++ "\t|"
                         ++ (si $ traitTotalScore  t) ++ "\t|"
printAbilityLine :: Trait -> String
printAbilityLine t = "| " ++ (ss $ traitLabel t) ++ "\t | " 
                         ++ (ss $ traitSpeciality t) ++ "\t | "
                         ++ (si $ traitScore t) ++ "\t| "
                         ++ (si $ traitXP  t) ++ "\t|"
                         ++ (si $ traitBonus  t) ++ "\t|"
                         ++ (si $ traitTotalScore  t) ++ "\t|"








ss :: Maybe String -> String
ss (Just s) = s
ss Nothing = "-"
si :: Maybe Int -> String
si (Just s) = show s
si Nothing = "-"
