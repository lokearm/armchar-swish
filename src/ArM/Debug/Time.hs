{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Debug.Time
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Rudimentary functions to log CPU time.
--
-- Simple functions to log CPU usage.
--
-----------------------------------------------------------------------------

module ArM.Debug.Time where

import Numeric
import System.CPUTime

showf :: Integer -> String
showf f = showFFloat (Just 3) t1 "" 
            where t1 = fromIntegral f * 10**(-12) :: Double
printTime :: IO ()
printTime = do
     t1 <- getCPUTime
     print $ "Total CPU Time: " ++ showf t1 ++ "s"
