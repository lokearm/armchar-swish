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
import Data.Maybe

getParam :: Character -> [ VF ] -> CharGenParameters 
getParam c _ | isGrog c = p { vfLimit = 3 }
             | otherwise = p
             where p = defaultParam

getInitVF :: Character -> [ VF ]
getInitVF = getInitVF' . head . pregameAdvancement

getInitVF' :: Advancement -> [ VF ]
getInitVF' a | m /= "Virtues and Flaws" = []
            | otherwise = getVF $ changes a
           where m = fromMaybe "" $ mode a
  

data CharGenParameters = CharGenParameters 
     { vfLimit :: Int
     , charLimit :: Int
     , ecLimit :: Int
     , apprenticeshipLimit :: Int
     }

defaultParam :: CharGenParameters 
defaultParam = CharGenParameters 
     { vfLimit = 10
     , charLimit = 7 
     , ecLimit = 45
     , apprenticeshipLimit = 240
     }

-- | Augment and amend the advancements based on current virtues and flaws.
prepareCharGen :: [VF] -> Advancement -> AugmentedAdvancement
prepareCharGen vfs = validate . initialLimits vfs . addInferredTraits 


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
applyCharGenAdv :: [VF] -> Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyCharGenAdv vfs a cs = (a',cs')
    where a' = prepareCharGen vfs a
          cs' = cs { charTime = season a, traits = new }
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
    where (a',cs') = applyCharGenAdv vfs y cs
          vfs = vfList $ filterCS cs

