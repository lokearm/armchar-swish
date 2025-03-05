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

import Data.Maybe (fromMaybe)

-- | Get a list from a maybe-list, mapping Nothing to the empty list.
maybeList :: Maybe [a] -> [a]
maybeList = fromMaybe []

-- | Get a string from a maybe-string, mapping Nothing to the empty string.
maybeString :: Maybe String -> String
maybeString = fromMaybe ""

-- | Get a number from a maybe-number, mapping Nothing to zero.
maybeInt :: Num a => Maybe a -> a
maybeInt = fromMaybe 0

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

-- | Show a non-zero integer with sign, or an empty string
showBonus :: Int -> String
showBonus x | x > 0 = " +" ++ show x
            | x < 0 = " " ++ show x
            | otherwise = ""

-- | Show an integer with sign
showSigned :: Int -> String
showSigned x | x > 0 = "+" ++ show x
            | otherwise = show x
