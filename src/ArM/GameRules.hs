-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.GameRules
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Simple utilities for basic calculation in ArM Rules.
--
-- Simple and generic functions for ArM Rules.  Should be independent
-- of the software design.
--
-----------------------------------------------------------------------------
module ArM.GameRules where

import Data.Maybe

type XPType = Int

-- | Calculate score from total XP, using the arts scale.
-- For abilities, the argument should be divided by 5 beforehand.
scoreFromXP :: XPType -> Int
scoreFromXP y = floor $ (-1+sqrt (1+8*x))/2
    where x = fromIntegral y  :: Double

pyramidScore :: Int -> XPType
pyramidScore y | y < 0 = y*(-y+1) `div` 2
               | otherwise = y*(y+1) `div` 2

getAbilityScore :: Maybe XPType -> (Int,XPType)
getAbilityScore x' = (s,y) 
     where y = x - 5*pyramidScore s
           s = scoreFromXP (x `div` 5)
           x = fromMaybe 0 x'

