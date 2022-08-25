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
import ArM.Types.Saga

data CharStage = CharStage 
     { stage :: String    
       -- ^ Stage of development, e.g. Early Childhood
     , advancement :: Maybe Advancement  
       -- ^ The advancement of this stage
     , sheetObject :: CharacterSheet     
       -- ^ The resulting character
     , sheetGraph :: RDFGraph 
       -- ^ The character sheet as RDF Graph
     }  deriving (Eq)
data CharGen = CharGen 
      { charID :: RDFLabel      -- ^ Character ID 
      , charName :: String      -- ^ Character Name (for display purpose)
      , rawGraph :: RDFGraph    -- ^ Raw graph as stored on file
      , charGraph :: RDFGraph   -- ^ Augmented graph with inference
      , baseSheet :: CharacterSheet 
        -- ^ Character Sheet at the start of the process
      , charSheets :: [CharStage]  
        -- ^ List of development stages, most recent first
      }  deriving (Eq)
instance Show CharStage where
    show cs = stage cs ++ show (advancement cs)
instance Show CharGen where
    show cs = charName cs ++ " (" ++ (show $ charID cs) ++ ")"

-- |
-- = Keys
data CharacterKey = CharacterKey {
            keyYear :: Int,
            keySeason :: String,
            keyChar :: String } deriving (Ord,Eq,Show)

getKey :: CharacterSheet -> CharacterKey
getKey cs = CharacterKey { keyYear = case (csYear cs) of
                                Nothing -> 0
                                (Just y) -> y,
                           keySeason = (csSeason cs),
                           keyChar = show $ csID cs }

getAdvFiles ft s = getFiles "hasAdvancementFile"
