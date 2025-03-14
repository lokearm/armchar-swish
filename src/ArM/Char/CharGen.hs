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
-- The module exports only one function, `prepareCharacter` which 
-- computes the starting character using the list of pregame
-- advancements.
--
-----------------------------------------------------------------------------
module ArM.Char.CharGen (prepareCharacter) where

import ArM.Char.Trait
import ArM.Char.Internal.Character
import ArM.Char.Internal.Advancement
import ArM.Char.CharacterSheet
import ArM.Char.Advancement
import ArM.Char.Validation
import ArM.Char.Virtues
import ArM.GameRules
import Data.Maybe

import ArM.Debug.Trace

-- | Compute the initial state if no state is recorded.
--
-- The function uses `applyCGA` to process all of the pregame advancements.
-- It then calls `addConfidence` to add the confidence trait to the state
-- for the returned `Character` object
prepareCharacter :: Character -> Character
prepareCharacter c 
            | state c /= Nothing = c
            | otherwise = c { state = newstate
                            , pregameDesign = xs
                            , pregameAdvancement = []
                            }
            where as = pregameAdvancement  c 
                  (xs,cs) = applyCGA as defaultCS { charSType = charType $ concept c }
                  newstate = Just $ addConfidence $ cs { charTime = GameStart }

-- | Augment and amend the advancements based on current virtues and flaws.
--
-- This function is applied by `applyCharGenAdv` before the advancement is
-- applied to the `CharacterState`.  It infers additional traits from 
-- virtues and flaws, add XP limits to the advancements, and checks that
-- the advancement does not overspend XP or exceed other limnits.
prepareCharGen :: CharacterState -> Advancement -> AugmentedAdvancement
prepareCharGen cs = validateCharGen sheet . initialLimits vfs . addInferredTraits 
          where vfs = vfList sheet
                sheet = filterCS cs

-- | Add the Confidence trait to the character state, using 
addConfidence :: CharacterState -> CharacterState
addConfidence cs = cs { traits = sortTraits $ ct:traits cs }
          where vfs = vfList sheet
                sheet = filterCS cs
                ct | csType sheet == Grog = ConfidenceTrait $ Confidence
                           { cname = "Confidence", cscore = 0, cpoints = 0 }
                   | otherwise = inferConfidence vfs 


-- | Apply CharGen advancement
applyCharGenAdv :: Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyCharGenAdv a cs = (a',cs')
    where a' = prepareCharGen cs a
          cs' = trace ("old>"++show old)
              $ trace ("change>"++show change)
              $ trace ("new>"++show new)
              $ cs { charTime = season a, traits = new, age = ag }
          new =  advance change tmp
          tmp =  advance inferred old 
          change = sortTraits $ changes a'
          inferred = inferredTraits a'
          old = traits cs
          ag = fromMaybe 0 (augYears a') + age cs

-- | Apply a list of advancements
applyCGA :: [Advancement] -> CharacterState -> ([AugmentedAdvancement],CharacterState)
applyCGA a cs = applyCGA' ([],a,cs)

-- | Recursive helper for `applyCGA`.
applyCGA' :: ([AugmentedAdvancement],[Advancement],CharacterState)
                   -> ([AugmentedAdvancement],CharacterState)
applyCGA' (xs,[],cs) = (xs,cs)
applyCGA' (xs,y:ys,cs) = applyCGA' (a':xs,ys,cs')
    where (a',cs') = applyCharGenAdv y cs

-- |
-- = Validation

-- | validate an advancement, adding results to the validation field
validateCharGen :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateCharGen cs a 
           | m == "Virtues and Flaws" = validateVF cs a
           | m == "Characteristics" = validateChar cs a
           | otherwise = validateLevels $ validateXP a
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

-- | Return the limit on flaw points, i.e. 3 for grogs and 10 for others.
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
                 lim = getCharAllowance $ vfList sheet
                 ex = calculateCharPoints $ advancement a
                 und = "Underspent " ++ (show ex) ++ " points out of "
                     ++ show lim ++ " on characteristics."  
                 over = "Overspent " ++ (show ex) ++ " points out of "
                     ++ show lim ++ " on characteristics."  
                 val = "Correctly spent " ++ (show ex) ++ " points on characteristics."  

-- | Count characterics points spent in an Advancement
calculateCharPoints :: Advancement -> Int
calculateCharPoints = sum . map cScore . changes

-- | Count characterics points spent on a trait
cScore :: ProtoTrait -> Int
cScore p | isJust (characteristic p) = f p
            | otherwise = 0
        where f = pyramidScore . fromMaybe 0 . score 

-- | Calculate initial XP limits on Char Gen Advancements
initialLimits :: [ VF ] -> AugmentedAdvancement -> AugmentedAdvancement
initialLimits vfs ad
            | m == "Early Childhood" = ( f ad 45 ) { augYears = Just 5 }
            | m == "Apprenticeship" = app ad
            | m == "Characteristics" = f ad 0
            | m == "Later Life" = f ad $ laterLifeSQ vfs (advancement ad)
            | otherwise = ad { effectiveSQ = sourceQuality $ advancement ad  }
           where m = fromMaybe "" $ mode ad
                 f a x | isJust t = a { effectiveSQ = t }
                       | otherwise = a { effectiveSQ = Just x }
                 t = sourceQuality $ advancement ad
                 (app1,app2) = appSQ vfs
                 app a = a { effectiveSQ = Just app1, levelLimit = Just app2, augYears = Just 15 }

-- | Validate allocation of Spell Levels.
validateLevels :: AugmentedAdvancement -> AugmentedAdvancement
validateLevels a | isNothing (levelLimit a) = a
                 | sq > lsum = a { validation = und:validation a }
                 | sq < lsum = a { validation = over:validation a }
                 | otherwise = a { validation = val:validation a }
    where lsum = calculateLevels $ advancement a
          sq = fromMaybe 0 $ levelLimit a
          val = Validated $ "Correctly spent " ++ show sq ++ " spell levels."
          over = ValidationError $ "Overspent " ++ show lsum ++ " spell levels of " ++ show sq ++ "."
          und = ValidationError $ "Underspent " ++ show lsum ++ " spell levels of " ++ show sq ++ "."

-- | Count spell levels from an Advancement
calculateLevels :: Advancement -> Int
calculateLevels = sum . map ( fromMaybe 0 . level ) . changes
