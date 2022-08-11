{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.CharacterMap
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- A Map to store character sheets.
-- Each character sheet is stored as one RDFGraph (embedded in the
-- `CharacterRecord` type).
-- This is an auxiliary for STM and should normally not be imported elsewhere
--
-----------------------------------------------------------------------------
module ArM.STM.CharacterMap ( CharacterRecord(..)
                            , CharacterMap
                            , insertListS
                            , empty 
                            , ArM.STM.CharacterMap.lookup 
                            ) where

import qualified Data.Map as M
import qualified Swish.RDF.Graph as G
import qualified ArM.Character as C
import qualified ArM.Rules.Record as R
import           Data.Maybe (fromJust)
import ArM.Resources 
import Control.Parallel.Strategies (parMap,rpar)

type CharacterMap = M.Map CharacterKey CharacterRecord
data CharacterKey = CharacterKey {
            keyYear :: Int,
            keySeason :: String,
            keyChar :: String } deriving (Ord,Eq,Show)
data CharacterRecord = CharacterRecord G.RDFGraph
    deriving Show

getKey :: C.CharacterSheet -> CharacterKey
getKey cs = CharacterKey { keyYear = case (C.csYear cs) of
                                Nothing -> 0
                                (Just y) -> y,
                           keySeason = (C.csSeason cs),
                           keyChar = show $ C.csID cs }

-- | Prepare a list of character sheets with a given schema and insert into the map 
insertListS :: G.RDFGraph -> CharacterMap -> [C.CharacterSheet] -> CharacterMap
insertListS schema cmap cs = foldl ins cmap $ parMap rpar ff cs
   where f = (CharacterRecord . R.prepareRecord schema . C.makeRDFGraph)
         ff x = (f x, getKey x)
         ins cmap x = M.insert (snd x) (fst x) cmap

empty :: CharacterMap
empty = M.empty
lookup :: CharacterMap -> String -> String -> Int -> Maybe CharacterRecord
lookup cmap c s y = M.lookup ck cmap
   where ck = CharacterKey { keyYear = y, keySeason = s, keyChar = c }

