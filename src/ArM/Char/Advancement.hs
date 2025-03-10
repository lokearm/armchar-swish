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
                           , getVF
                           , parseSeasonTime
                           , SeasonTime(..)
                           , Advancement(..)
                           , AugmentedAdvancement(..)
                           , AdvancementLike(..)
                           , prepareAdvancement
                           ) where

-- import ArM.Char.Character
-- import ArM.Helper
import ArM.Char.Trait
import ArM.Char.Virtues
import ArM.Char.Internal.Advancement
import ArM.Char.Internal.Character
import ArM.Char.Validation

import Data.Maybe 




addInferredTraits :: Advancement -> AugmentedAdvancement
addInferredTraits a = defaultAA { inferredTraits = f a
                                , advancement = a
                                , augYears = advYears a }
     where f = inferTraits . getVF . changes 

-- | Get the virtues and flaws from a list of ProtoTrait objects, and convert them to
-- VF objects
getVF :: [ ProtoTrait ] -> [ VF ]
getVF [] = []
getVF (p:ps) | isJust (virtue p) = g p:getVF ps
             | isJust (flaw p) = g p:getVF ps
             | otherwise = getVF ps
    where g = fromJust . computeTrait

-- | Calculate initial XP limits on Char Gen Advancements
inferSQ :: AugmentedAdvancement -> AugmentedAdvancement
inferSQ ad = ad { effectiveSQ = sourceQuality $ advancement ad  }

-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: CharacterState -> Advancement -> AugmentedAdvancement
prepareAdvancement _ = validate . inferSQ . addInferredTraits
