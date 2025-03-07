-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Validation
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Functions to calculate and validate advancements
--
-----------------------------------------------------------------------------
module ArM.Char.Validation (validate) where

-- import ArM.Char.Character
-- import ArM.Helper
import ArM.GameRules
import ArM.Char.Internal.Advancement
import ArM.Char.Trait

import Data.Maybe (fromMaybe,isJust)



-- | Count virtue and flaw costs from an Advancement
calculateVFCost :: Advancement -> (Int,Int)
calculateVFCost a = ( sum $ filter (<0) rs, sum $ filter (>0) rs )
   where rs = map regCost $ changes a



regCost :: ProtoTrait -> Int
regCost p | isJust (virtue p) = f p
          | isJust (flaw p) = f p
          | otherwise = 0
        where f = fromMaybe 0 . cost 

-- | Count regular XP (excluding reputation) from a ProtoTrait
regXP :: ProtoTrait -> Int
regXP p | isJust (ability p) = f p
        | isJust (art p) = f p
        | isJust (spell p) = f p
        | otherwise = 0
        where f = fromMaybe 0 . xp 



validate :: AugmentedAdvancement -> AugmentedAdvancement
validate a | m == "Virtues and Flaws" = validateVF a
           | m == "Characteristics" = validateChar a
           | otherwise = validateXP a
           where m = fromMaybe "" $ mode a

validateVF :: AugmentedAdvancement -> AugmentedAdvancement
validateVF a | m /= "Virtues and Flaws" = a
             | 0 /= f + v = a { validation = ValidationError imb:validation a }
             | v > 10 = a { validation = ValidationError over:validation a }
             | otherwise = a { validation = Validated val:validation a }
           where m = fromMaybe "" $ mode a
                 (f,v) = calculateVFCost $ advancement a
                 imb = "Virtues and flaws are imbalanced: "
                     ++ show v ++ " points of virtues and"
                     ++ show (-f) ++ " points of flaws."
                 over = "Exceeds limit on virtues; " ++ show v ++ " points."
                 val = "Virtues and flaws balance at " ++ show v ++ " points."


                

validateXP :: AugmentedAdvancement -> AugmentedAdvancement
validateXP a | sq > xpsum = a { validation = und:validation a }
             | sq < xpsum = a { validation = over:validation a }
             | otherwise = a { validation = val:validation a }
    where xpsum = calculateXP $ advancement a
          sq = fromMaybe 0 $ effectiveSQ a
          val = Validated $ "Correctly spent " ++ show sq ++ " xp."
          over = ValidationError $ "Overspent " ++ show xpsum ++ "xp of " ++ show sq ++ "."
          und = ValidationError $ "Underspent " ++ show xpsum ++ "xp of " ++ show sq ++ "."

--
-- | Count regular XP (excluding reputation) from an Advancement
calculateXP :: Advancement -> Int
calculateXP = sum . map regXP . changes

-- | Validate points spent on characterics.
validateChar :: AugmentedAdvancement -> AugmentedAdvancement
validateChar a | m /= "Characteristics" = a
             | ex < lim = a { validation = ValidationError und:validation a }
             | ex > lim = a { validation = ValidationError over:validation a }
             | otherwise = a { validation = Validated val:validation a }
           where m = fromMaybe "" $ mode a
                 lim = 7
                 ex = calculateCharPoints $ advancement a
                 und = "Underspent " ++ (show ex) ++ " points on characteristics."  
                 over = "Underspent " ++ (show ex) ++ " points on characteristics."  
                 val = "Correctly spent " ++ (show ex) ++ " points on characteristics."  

-- | Count characterics points spent in an Advancement
calculateCharPoints :: Advancement -> Int
calculateCharPoints = sum . map charScore . changes

-- | Count characterics points spent on a trait
charScore :: ProtoTrait -> Int
charScore p | isJust (characteristic p) = f p
            | otherwise = 0
        where f = pyramidScore . fromMaybe 0 . score 
