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
-- Each character sheet is stored as one RDFGraph.
--
-----------------------------------------------------------------------------
module ArM.CharacterMap where

import qualified Data.Map as M
import qualified Swish.RDF.Graph as G
import qualified ArM.Character as C
import Data.Maybe (fromJust)

type CharacterMap = M.Map CharacterKey CharacterRecord
data CharacterKey = CharacterKey {
            keyYear :: Int,
            keySeason :: String,
            keyChar :: String } deriving (Ord,Eq,Show)
data CharacterRecord = CharacterRecord G.RDFGraph


insert :: CharacterMap -> C.CharacterSheet -> CharacterMap
insert cmap cs = M.insert (getKey cs) cr cmap
    where cr = CharacterRecord $ C.csToRDFGraph cs

getKey :: C.CharacterSheet -> CharacterKey
getKey cs = CharacterKey { keyYear = fromJust $ C.csYear cs,
                           keySeason = show $ C.csSeason cs,
                           keyChar = C.csID cs }

insertList :: CharacterMap -> [C.CharacterSheet] -> CharacterMap
insertList cmap cs = foldl insert cmap cs
