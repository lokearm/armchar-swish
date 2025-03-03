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

-- | Calculate score from total XP, using the arts scale.
-- For abilities, the argument should be divided by 5 beforehand.
scoreFromXP :: Int -> Int
scoreFromXP y = floor $ (-1+sqrt (1+8*x))/2
    where x = fromIntegral y  :: Double

xpFromScore :: Int -> Int
xpFromScore y = y*(y+1) `div` 2
