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

-- | The XPType is the taype of XP.
-- By RAW it should be Int, but some troupes prefer to count
-- fractional XP when affinities are involved.  This requires
-- changing to a floating point type.
-- type XPType = Float
type XPType = Int

-- | Round the XPType if required.  
-- This has to be redefined depending on the type of XPType.
xpround :: Float -> XPType
-- xpround = id
xpround = round


-- | Calculate score from total XP, using the arts scale.
-- For abilities, the argument should be divided by 5 beforehand.
scoreFromXP :: XPType -> Int
scoreFromXP y = floor $ (-1+sqrt (1+8*x))/2
    where x = fromIntegral y  :: Double

pyramidScore :: Int -> XPType
pyramidScore = fromIntegral . f
  where  f y | y < 0 = y*(-y+1) `div` 2
             | otherwise = y*(y+1) `div` 2

getAbilityScore :: Maybe XPType -> (Int,XPType)
getAbilityScore x' = (s,y) 
     where y = x - 5*pyramidScore s
           s = scoreFromXP (x `div` 5)
           x = fromMaybe 0 x'
