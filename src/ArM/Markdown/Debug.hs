{-# LANGUAGE OverloadedStrings #-}
module ArM.Markdown.Debug where

import Swish.RDF.Graph as G
import ArM.KeyPair()
import           ArM.KeyPair
import           ArM.Sheet.SheetObject
import           ArM.Swish.CharacterQuery
import Data.List(sortOn,intercalate)

tuttishow :: KeyPairList -> String
tuttishow (KeyPairList ls) = show ls
-- printArtLine :: KeyPairList -> String

-- Debug
debugChar :: RDFGraph -> [String]
debugChar = map tuttishow . getCharacteristics
