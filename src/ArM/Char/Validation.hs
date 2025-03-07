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
module ArM.Char.Validation where

-- import ArM.Char.Character
-- import ArM.Helper
import ArM.GameRules
import ArM.Char.Advancement
import ArM.Char.Trait

import Data.Maybe (fromMaybe,isJust)


-- | Count regular XP (excluding reputation) from an Advancement
calculateXP :: Advancement -> Int
calculateXP = sum . map regXP . changes

-- | Count virtue and flaw costs from an Advancement
calculateVFCost :: Advancement -> (Int,Int)
calculateVFCost a = ( sum $ filter (<0) rs, sum $ filter (>0) rs )
   where rs = map regCost $ changes a

-- | Count characterics points spent in an Advancement
calculateCharPoints :: Advancement -> Int
calculateCharPoints = sum . map pyramidScore . map charScore . changes

charScore :: ProtoTrait -> Int
charScore p | isJust (characteristic p) = f p
            | otherwise = 0
        where f = fromMaybe 0 . score 

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
             | v /= f = a { validation = Error imb:validation a }
             | v > 10 = a { validation = Error over:validation a }
             | otherwise = a { validation = Validated val:validation a }
           where m = fromMaybe "" $ mode a
                 (v,f) = calculateVFCost $ advancement a
                 imb = "Virtues and flaws are imbalanced: "
                     ++ show v ++ " points of virtues and"
                     ++ show (-f) ++ " points of flaws."
                 over = "Exceeds limit on virtues; " ++ show v ++ " points."
                 val = "Virtues and flaws balance at " ++ show v ++ " points."


                

validateXP :: AugmentedAdvancement -> AugmentedAdvancement
validateXP a = a

validateChar :: AugmentedAdvancement -> AugmentedAdvancement
validateChar a = a

