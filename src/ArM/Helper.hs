-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Helper
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Generic and simple helper functions
--
--
-----------------------------------------------------------------------------
module ArM.Helper where

-- | Get a list from a maybe-list, mapping Nothing to the empty list.
maybeList :: Maybe [a] -> [a]
maybeList Nothing = []
maybeList (Just x) = x

-- | Get a string from a maybe-string, mapping Nothing to the empty string.
maybeString :: Maybe String -> String
maybeString Nothing = ""
maybeString (Just x) = x

-- | Get a number from a maybe-number, mapping Nothing to zero.
maybeInt :: Num a => Maybe a -> a
maybeInt Nothing = fromInteger 0
maybeInt (Just x) = x

-- | Add maybe-integers, treating Nothing as zero.
maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y = Just $ maybeInt x + maybeInt y

-- | Remove Nothing elements from a list, and map the Just-elements
-- to the constituent object.
filterNothing :: [Maybe a] -> [a]
filterNothing = f
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs

maybeShow :: Show a => Maybe a -> String
maybeShow Nothing = ""
maybeShow (Just x) = show x
