-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.NoTrace
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- This module defines the same functions as `ArM.Trace` to be identity
-- functions; thus the debug trace can be turned off simply by importing
-- this module instead of `ArM.Trace`, leaving the debug code in place.
--
-----------------------------------------------------------------------------

module ArM.NoTrace where


trace :: String -> a -> a
trace _ y = y
ttrace :: Show a => a -> a
ttrace x = x
strace :: String -> String
strace x = x
