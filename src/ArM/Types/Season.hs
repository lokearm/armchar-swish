{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Season
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
--
-----------------------------------------------------------------------------
module ArM.Types.Season where

import Data.Maybe (fromJust)

data CharTime = CharTime 
                { charYear :: Maybe Int
                , charSeason :: String
                , advancementStage :: String
                , advancementIndex :: Int 
                } deriving (Show)
defaultCharTime :: CharTime 
defaultCharTime = CharTime 
                { charYear = Nothing
                , charSeason = ""
                , advancementStage = ""
                , advancementIndex = 0
                } 
instance Eq CharTime where
  (==) x y | (charYear x == charYear y) && (charSeason x == charSeason y) = True
           | x == y = True
           | otherwise = False
instance Ord CharTime where
      compare x y | (charYear x < charYear y) = LT
                  | (charYear x > charYear y) = GT
                  | charYear x == Nothing = idxcompare x y
                  | (f x < f y) = LT
                  | (f x > f y) = GT
                  | otherwise  = EQ
         where f = seasonNo . charSeason
               idxcompare x1 x2 
                  | (advancementIndex x1 < advancementIndex x2) = LT
                  | (advancementIndex x1 > advancementIndex x2) = GT
                  | otherwise = EQ

-- |
-- = Season


-- | The `SeasonYear` gives a season with year.
type SeasonYear = (String,Int) 
-- | Given a season/year, `nextSeason` returns the subsequent season.
-- Winter is the last season of the year, and is followed by the Spring of
-- the next year.
nextSeason :: SeasonYear -> SeasonYear
nextSeason ("Spring",y) = ("Summer",y)
nextSeason ("Summer",y) = ("Autumn",y)
nextSeason ("Autumn",y) = ("Winter",y)
nextSeason ("Winter",y) = ("Spring",y+1)
nextSeason ("",y) = ("",y+1)
nextSeason (_,_) = error "Invalid Season in nextSeason"

nextCharTime :: CharTime -> CharTime
nextCharTime x | charYear x == Nothing = x
nextCharTime x | otherwise = x { charYear = Just y', charSeason = s' }
       where (s',y') = nextSeason (charSeason x, fromJust (charYear x))


-- | Given a season as a String, `nextSeason` returns a number by which 
-- seasons can be ordered within a calendar year.
-- Winter is the last season in the year.
seasonNo :: String -> Int
seasonNo "Spring" = 1
seasonNo "Summer" = 2
seasonNo "Autumn" = 3
seasonNo "Winter" = 4
seasonNo _ = 10

maybeNextSeason :: (String,Maybe Int) ->  (String,Maybe Int)
maybeNextSeason ("",Just y) = ("",Just (y+1)) 
maybeNextSeason ("",Nothing) = ("",Nothing) 
maybeNextSeason (s,Just y) = (s',Just y') where (s',y') = nextSeason (s,y)
maybeNextSeason (s,Nothing) = (s',Nothing) where (s',_) = nextSeason (s,0) 

class HasTime a where
    timeOf :: a -> CharTime
    hasYear :: a -> Maybe Int
    hasYear = charYear . timeOf
    hasSeason :: a -> String
    hasSeason = charSeason . timeOf

