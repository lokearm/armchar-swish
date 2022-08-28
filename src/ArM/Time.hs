{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Time
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Simple functions to log CPU usage.
--
-----------------------------------------------------------------------------

module ArM.Time where
import Numeric
import System.CPUTime

showf :: Integer -> String
showf f = showFFloat (Just 3) t1 "" 
            where t1 = fromIntegral f * 10**(-12) :: Double
printTime :: IO ()
printTime = do
     t1 <- getCPUTime
     print $ "Total CPU Time: " ++ showf t1 ++ "s"
