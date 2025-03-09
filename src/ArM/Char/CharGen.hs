-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.CharGen
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Character Generation Management
--
-----------------------------------------------------------------------------
module ArM.Char.CharGen where

import ArM.Char.Trait
import ArM.Char.Internal.Character
import ArM.Char.Internal.Advancement
import ArM.Char.CharacterSheet
import ArM.Char.Advancement
import ArM.Char.Validation
import ArM.GameRules
import Data.Maybe

import ArM.Debug.Trace

-- | Augment and amend the advancements based on current virtues and flaws.
prepareCharGen :: CharacterState -> Advancement -> AugmentedAdvancement
prepareCharGen cs = validateCharGen sheet . initialLimits vfs . addInferredTraits 
          where vfs = vfList sheet
                sheet = filterCS cs


-- | Compute the initial state if no state is recorded.
prepareCharacter :: Character -> Character
prepareCharacter c 
            | state c /= Nothing = c
            | otherwise = c { state = Just $ cs { charTime = Just "Game start" }
                            , pregameDesign = xs
                            , pregameAdvancement = []
                            }
            where as = pregameAdvancement  c 
                  (xs,cs) = applyCGA as defaultCS


-- | Apply CharGen advancement
applyCharGenAdv :: Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyCharGenAdv a cs = (a',cs')
    where a' = prepareCharGen cs a
          cs' = trace (show $ mode a) $ cs { charTime = season a, traits = new }
          new =  advance change tmp
          tmp =  advance inferred old 
          change = sortTraits $ changes a'
          inferred = inferredTraits a'
          old = traits cs

-- | Apply a list of advancements
applyCGA :: [Advancement] -> CharacterState -> ([AugmentedAdvancement],CharacterState)
applyCGA a cs = applyCGA' ([],a,cs)
applyCGA' :: ([AugmentedAdvancement],[Advancement],CharacterState)
                   -> ([AugmentedAdvancement],CharacterState)
applyCGA' (xs,[],cs) = (xs,cs)
applyCGA' (xs,y:ys,cs) = applyCGA' (a':xs,ys,cs')
    where (a',cs') = trace ("cs "++show cs) $ 
               trace ("sheet "++show (filterCS cs)) $ applyCharGenAdv y cs

-- |
-- = Validation

-- | validate an advancement, adding results to the validation field
validateCharGen :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateCharGen cs a 
           | m == "Virtues and Flaws" = validateVF cs a
           | m == "Characteristics" = validateChar cs a
           | otherwise = validateXP a
           where m = fromMaybe "" $ mode a

-- | Validate allocation of virtues and flaws.
validateVF :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateVF sheet a 
             | m /= "Virtues and Flaws" = a
             | 0 /= f + v = a { validation = ValidationError imb:validation a }
             | v > lim = a { validation = ValidationError over:validation a }
             | otherwise = a { validation = Validated val:validation a }
           where m = fromMaybe "" $ mode a
                 (f,v) = calculateVFCost $ advancement a
                 imb = "Virtues and flaws are imbalanced: "
                     ++ show v ++ " points of virtues and"
                     ++ show (-f) ++ " points of flaws."
                 over = "Exceeds limit on virtues; " ++ show v ++ suf
                 val = "Virtues and flaws balance at " ++ show v ++ suf
                 suf = " of " ++ show lim ++ " points."
                 lim = vfLimit sheet
vfLimit :: CharacterSheet -> Int
vfLimit sheet | Grog == csType sheet = 3
              | otherwise = 10

-- | Count virtue and flaw costs from an Advancement
calculateVFCost :: Advancement -> (Int,Int)
calculateVFCost a = ( sum $ filter (<0) rs, sum $ filter (>0) rs )
   where rs = map regCost $ changes a


-- | Extract the virtue/flaw cost from a ProtoType; zero for other types of traits.
regCost :: ProtoTrait -> Int
regCost p | isJust (virtue p) = f p
          | isJust (flaw p) = f p
          | otherwise = 0
        where f = fromMaybe 0 . cost 

-- | Validate points spent on characterics.
validateChar :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateChar sheet a | m /= "Characteristics" = a
             | ex < lim = a { validation = ValidationError und:validation a }
             | ex > lim = a { validation = ValidationError over:validation a }
             | otherwise = a { validation = Validated val:validation a }
           where m = fromMaybe "" $ mode a
                 lim = fromMaybe 0 $ charAllowance a
                 ex = calculateCharPoints $ advancement a
                 und = "Underspent " ++ (show ex) ++ " points out of "
                     ++ show lim ++ " on characteristics."  
                 over = "Underspent " ++ (show ex) ++ " points out of "
                     ++ show lim ++ " on characteristics."  
                 val = "Correctly spent " ++ (show ex) ++ " points on characteristics."  

-- | Count characterics points spent in an Advancement
calculateCharPoints :: Advancement -> Int
calculateCharPoints = sum . map charScore . changes

-- | Count characterics points spent on a trait
charScore :: ProtoTrait -> Int
charScore p | isJust (characteristic p) = f p
            | otherwise = 0
        where f = pyramidScore . fromMaybe 0 . score 
