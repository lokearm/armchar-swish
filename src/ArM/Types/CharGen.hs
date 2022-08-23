{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.CharGen
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle characters as stored in web server memory.
--
-----------------------------------------------------------------------------
module ArM.Types.CharGen where

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe
import ArM.KeyPair
import ArM.Resources
import ArM.BlankNode
import ArM.Rules.Aux
import ArM.Types.Character

data CharStage = CharStage 
     { stage :: String    
       -- ^ Stage of development, e.g. Early Childhood
     , advancement :: Maybe Advancement  
       -- ^ The advancement of this stage
     , sheetObject :: CharacterSheet     
       -- ^ The resulting character
     , sheetGraph :: CharacterRecord 
       -- ^ The character sheet as RDF Graph
     }  deriving (Eq)
data CharGen = CharGen 
      { charID :: RDFLabel
      , charName :: String
      , rawGraph :: RDFGraph    -- ^ Raw graph as stored on file
      , charGraph :: RDFGraph   -- ^ Augmented graph with inference
      , charSheets :: [CharStage]
      }  deriving (Eq)
instance Show CharStage where
    show cs = stage cs ++ show (advancement cs)
instance Show CharGen where
    show cs = charName cs ++ " (" ++ (show $ charID cs) ++ ")"

data CharacterRecord = CharacterRecord G.RDFGraph
    deriving (Show,Eq)

type CharacterMap = M.Map CharacterKey TCG.CharacterRecord

-- |
-- = Keys
data CharacterKey = CharacterKey {
            keyYear :: Int,
            keySeason :: String,
            keyChar :: String } deriving (Ord,Eq,Show)

getKey :: C.CharacterSheet -> CharacterKey
getKey cs = CharacterKey { keyYear = case (C.csYear cs) of
                                Nothing -> 0
                                (Just y) -> y,
                           keySeason = (C.csSeason cs),
                           keyChar = show $ C.csID cs }
