{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Time
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- These auxiliary functions were made to log CPU usage, but it does
-- not work very well due to Haskell's laziness.
--
-----------------------------------------------------------------------------

module ArM.Time where
import Numeric
import System.CPUTime

showf :: Integer -> String
showf f = showFFloat (Just 3) t1 "" 
            where t1 = fromIntegral ( f `div` 10^9 ) * 10**(-3)
printTime :: IO ()
printTime = do
     t1 <- getCPUTime
     print $ "Total CPU Time: " ++ showf t1 ++ "s"
