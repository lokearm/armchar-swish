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
module ArM.Char.Advancement( module ArM.Char.Internal.Advancement
                           , prepareAdvancementVF
                           , augmentTotalXP
                           ) where

-- import ArM.Char.Character
-- import ArM.Helper
import ArM.Char.Trait
import ArM.Char.Virtues
import ArM.Char.Internal.Advancement
import ArM.Char.Validation

import Data.Maybe (fromJust,isJust,fromMaybe)



-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancementVF :: Advancement -> AugmentedAdvancement
prepareAdvancementVF a = validate $ defaultAA { inferredTraits = f a, advancement = a }
     where f = inferTraits . getVF . changes 

augmentTotalXP :: AugmentedAdvancement -> AugmentedAdvancement
augmentTotalXP a | m == "Early Childhood" = a { effectiveSQ = Just 45 }
                 | m == "Apprenticeship" = a { effectiveSQ = Just 240 }
                 | m == "Later Life" = a { effectiveSQ = Just $ y*15 }
                 | otherwise = a { effectiveSQ = Just 0 }
           where m = fromMaybe "" $ mode a
                 y = fromMaybe 0 $ advYears $ advancement a

-- | Get the virtues and flaws from a list of ProtoTrait objects, and convert them to
-- VF objects
getVF :: [ ProtoTrait ] -> [ VF ]
getVF [] = []
getVF (p:ps) | isJust (virtue p) = g p:getVF ps
             | isJust (flaw p) = g p:getVF ps
             | otherwise = getVF ps
    where g = fromJust . computeTrait
