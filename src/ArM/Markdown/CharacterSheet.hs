{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.CharacterSheet
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle characters as stored in web server memory.
--
-----------------------------------------------------------------------------
module ArM.Markdown.CharacterSheet ( printSheetObject, DashShow(..) ) where


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
                       ": " ++ (lf $ map dashShow $ size ob),
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
                  ++ [""]
                  ++ printEquipment ob
                  ++ [""]
                  ++ printVis ob
   where p1 x = fJ (traitLabel x) ++ " " ++ (dashShow $ traitTotalScore x)
         fJ Nothing = "???"
         fJ (Just x) = x
         f = (':':) . (' ':) . intercalate ", "  
         p2 x = fJ (traitAbbr x) ++ " " ++ (dashShow $ traitTotalScore x)
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
         f1 = dashShow . traitLabel
         f4 = dashShow . traitScore
         f5 = dashShow . traitXP
         f2 = dashShow . traitCastingScore

printMD :: (String,String) -> String
printMD (x, y) = x ++ "\n: " ++ y ++ "\n"

confFormat :: (Maybe Int, Maybe Int) -> String
confFormat (x,y) = dashShow x ++ " (" ++ dashShow y ++ ")"

printEquipment :: SheetObject -> [String]
printEquipment = p . equipment
    where
       p [] = []
       p xs = [ "# Equipment", "" ] ++ (map printEquipmentLine xs) ++ [ "" ]
printEquipmentLine :: Trait -> String
printEquipmentLine t = "+ " ++ f1 t ++ f2 t ++ pShow (traitDetail t)
   where f1 = dashShow . traitLabel
         f2 = pShow . traitQuantity
printVisLine :: Trait -> String
printVisLine t = "+ " ++ f2 t  ++ f1  (traitArt t) t ++ pShow (traitDetail t)
   where f1 Nothing x =  (dashShow . traitLabel) x
         f1 (Just x) _ =  x
         f2 = s2 . traitQuantity
         s2 Nothing = ""
         s2 (Just x) = show x ++ "p "
printVis :: SheetObject -> [String]
printVis = p . vis
    where
       p [] = []
       p xs = "# Vis":"": p' xs  ++ [ "" ]
       p' = map printVisLine 

printVFLine :: Trait -> String
printVFLine t = "+ " ++ f1 t ++ f3 t ++ " (" ++ f2 t ++ ")"
   where vfDetail Nothing = "" 
         vfDetail (Just s) = ": " ++ s
         f3 = vfDetail . traitDetail
         f1 = dashShow . traitLabel
         f2 = dashShow . traitTotalScore


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
          (dashShow $ traitLabel t) 
          (dashShow $ traitScore t) 
          (dashShow $ traitXP  t) 
          (dashShow $ traitBonus  t) 
          (dashShow $ traitTotalScore  t)
printAbilityLine :: Trait -> String
printAbilityLine t = pAb (dashShow $ traitLabel t) 
                         (dashShow $ traitSpeciality t) 
                         (dashShow $ traitScore t) 
                         (dashShow $ traitXP  t) 
                         (dashShow $ traitBonus  t) 
                         (dashShow $ traitTotalScore  t) 
printCombat :: Trait -> String
printCombat c = "+ " ++ f1 (traitLabel c) ++ ": "
     ++ "Init " ++ (dashShow $ traitInit c)
     ++ "; Atk " ++ (dashShow $ traitAtk c)
     ++ "; Dfn " ++ (dashShow $ traitDfn c)
     ++ "; Dam " ++ (dashShow $ traitDam c)
     ++ "; Range " ++ (dashShow $ traitRange c)
  where f1 Nothing = "???"
        f1 (Just s) = s

class Show a => DashShow a where
   dashShow :: Maybe a -> String
   dashShow Nothing = "-"
   dashShow (Just x) = show x
   pShow :: Maybe a -> String
   pShow Nothing = ""
   pShow (Just x) = " (" ++ show x ++ ")"
instance DashShow Int 
instance DashShow String where
   dashShow Nothing = "-"
   dashShow (Just x) = x
   pShow Nothing = ""
   pShow (Just x) = " (" ++ x ++ ")"
