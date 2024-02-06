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


import ArM.Types.SheetObject
import Data.List(intercalate)

import Text.Printf

getHeader :: SheetObject -> String
getHeader ob = "# " ++ name
   where md = metadata ob
         name = fst $ f ("",md)
         f (n,((x,y):xs)) | x == "Name" = (y,[])
                          | otherwise = f (n,xs)
         f (n,[]) = (n,[])
printSheetObject :: SheetObject -> [String]
printSheetObject ob = [ getHeader  ob , "" ]
                  ++ (map printMD $ metadata ob) 
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
                       pAb "Ability" "Speciality" "Score" "XP" "Bonus" "Effective",
                       pAb ":-" ":-" "-:" "-:" "-:" "-:"
                       ]
                  ++ (map printAbilityLine $ abilities ob)
                  ++ printArts ob
                  ++ printSpells ob
                  ++ [ "", "# Combat", "" ]
                  ++ (map printCombat $ combat ob)
                  ++ printEquipment ob
                  ++ printVis ob
   where p1 x = fJ (traitLabel x) ++ " " ++ (fI $ traitTotalScore x)
         fJ Nothing = "???"
         fJ (Just x) = x
         fI Nothing = "???"
         fI (Just x) = show x
         f = (':':) . (' ':) . intercalate ", "  
         p2 x = fJ (traitAbbr x) ++ " " ++ (fI $ traitTotalScore x)
         lf [] = "-"
         lf xs = intercalate ", " xs

printArts :: SheetObject -> [String]
printArts = p . arts
    where
       p [] = []
       p xs = [ "# Arts", "" ] ++ artHeader ++ (map printArtLine xs)
printSpells :: SheetObject -> [String]
printSpells = p . spells
    where
       p [] = []
       p xs = [ "# Spells", "" ] ++ (map printSpellLine xs)

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


printEquipment :: SheetObject -> [String]
printEquipment = p . equipment
    where
       p [] = []
       p xs = [ "# Equipment", "" ] ++ (map printEquipmentLine xs) ++ [ "" ]
printEquipmentLine :: Trait -> String
printEquipmentLine t = "+ " ++ f1 t 
   where f1 = ss . traitLabel
printVisLine :: Trait -> String
printVisLine t = "+ " ++ f1 t 
   where f1 = ss . traitLabel
printVis :: SheetObject -> [String]
printVis = p . vis
    where
       p [] = []
       p xs = [ "# Vis", "" ] ++ (map printVisLine xs) ++ [ "" ]

printVFLine :: Trait -> String
printVFLine t = "+ " ++ f1 t ++ f3 t ++ " (" ++ f2 t ++ ")"
   where vfDetail Nothing = "" 
         vfDetail (Just s) = ": " ++ s
         f3 = vfDetail . traitDetail
         f1 = ss . traitLabel
         f2 = si . traitTotalScore



artHeader :: [String]
artHeader = [ 
  pArt "Art" "Score" "XP" "Bonus" "Effective",
  pArt ":-" "-:" "-:" "-:" "-:" ]
pArt :: String -> String -> String -> String -> String -> String
pArt = printf "| %-10s | %5s | %4s | %5s | %9s |" 
pAb :: String -> String -> String -> String -> String -> String -> String
pAb = printf "| %-20s | %-15s | %5s | %4s | %5s | %9s |" 
printArtLine :: Trait -> String
printArtLine t = pArt
          (ss $ traitLabel t) 
          (si $ traitScore t) 
          (si $ traitXP  t) 
          (sp $ traitBonus  t) 
          (si $ traitTotalScore  t)
printAbilityLine :: Trait -> String
printAbilityLine t = pAb (ss $ traitLabel t) 
                         (ss $ traitSpeciality t) 
                         (si $ traitScore t) 
                         (si $ traitXP  t) 
                         (si $ traitBonus  t) 
                         (si $ traitTotalScore  t) 
printCombat :: Trait -> String
printCombat c = "+ " ++ f1 (traitLabel1 c) ++ f2 (traitLabel2 c) ++ ": "
     ++ "Init " ++ (si $ traitInit c)
     ++ "; Atk " ++ (si $ traitAtk c)
     ++ "; Dfn " ++ (si $ traitDfn c)
     ++ "; Dam " ++ (si $ traitDam c)
     ++ "; Range " ++ (si $ traitRange c)
  where f1 Nothing = "???"
        f1 (Just s) = s
        f2 Nothing = ""
        f2 (Just s) = "[" ++ s ++ "]"

ss :: Maybe String -> String
ss (Just s) = s
ss Nothing = "-"
si :: Maybe Int -> String
si (Just s) = show s
si Nothing = "-"
sp :: Maybe Int -> String
sp (Just s) = printf "%5i+"  s
sp Nothing = "    -"
