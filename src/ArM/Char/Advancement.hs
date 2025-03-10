{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  The Advancement types representing changes over a season.
--
-----------------------------------------------------------------------------
module ArM.Char.Advancement( addInferredTraits
                           , initialLimits
                           , getVF
                           , parseSeasonTime
                           , SeasonTime(..)
                           , Advancement(..)
                           , AugmentedAdvancement(..)
                           , AdvancementLike(..)
                           ) where

-- import ArM.Char.Character
-- import ArM.Helper
import ArM.Char.Trait
import ArM.Char.Virtues
import ArM.Char.Internal.Advancement
-- import ArM.Char.Validation

import Data.Maybe (fromJust,isJust,fromMaybe)




addInferredTraits :: Advancement -> AugmentedAdvancement
addInferredTraits a = defaultAA { inferredTraits = f a, advancement = a }
     where f = inferTraits . getVF . changes 

-- | Get the virtues and flaws from a list of ProtoTrait objects, and convert them to
-- VF objects
getVF :: [ ProtoTrait ] -> [ VF ]
getVF [] = []
getVF (p:ps) | isJust (virtue p) = g p:getVF ps
             | isJust (flaw p) = g p:getVF ps
             | otherwise = getVF ps
    where g = fromJust . computeTrait

initialLimits :: [ VF ] -> AugmentedAdvancement -> AugmentedAdvancement
initialLimits vfs ad
            | m == "Early Childhood" = ( f ad 45 ) { augYears = Just 5 }
            | m == "Apprenticeship" = ( f ad 240 ) { augYears = Just 15 }
            | m == "Characteristics" = f ad 0
            | m == "Later Life" = f ad $ laterLifeSQ vfs (advancement ad)
            | otherwise = ad { effectiveSQ = sourceQuality $ advancement ad  }
           where m = fromMaybe "" $ mode ad
                 f a x | isJust t = a { effectiveSQ = t }
                       | otherwise = a { effectiveSQ = Just x }
                 t = sourceQuality $ advancement ad
