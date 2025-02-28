{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.CharacterSheet
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Char.CharacterSheet ( CharacterSheet(..)
                           , advanceCharacter
                           , getInitialCS
                           ) where

import Swish.RDF.Graph as G
import           Data.List (sort)

import ArM.Char.Character
import Data.Aeson
import Data.Aeson.Key


-- | 
-- = Character Sheet

data CharacterSheet = CharacterSheet {
         charID :: String
         csTime :: CharTime,  -- ^ Current Year
         , charGlance :: KeyPairList
         , charData :: KeyPairList
         , vf :: [ Trait ]
         , characteristics :: [ Trait ]
         , abilities :: [ Trait ]
         , arts :: [ Trait ]
         , spells :: [ Trait ]
         , ptraits :: [ Trait ]
         , reputations :: [ Trait ]
         , confidence :: Trait 
         , otherTraits :: [ Trait ]
       }  deriving (Eq)
      }  deriving (Eq)
instance Show CharacterSheet where
    show cs = "**" ++ show (csID cs) ++ "**\n" 
           ++ "-- " ++ ( showSheetID ) cs ++ "\n"
           ++ "Traits:\n" ++ showw ( csTraits cs )
           ++ "Metadata Triples:\n" ++ show ( csMetadata cs )
        where showw [] = ""
              showw (x:xs) = "  " ++ show x ++ "\n" ++ showw xs
defaultCS :: CharacterSheet
defaultCS = CharacterSheet {
         csID = "N/A"
         , csTime = defaultCharTime,
         . protoTraits = []
         , charGlance = []
         , charData = []
         , vf = []
         , characteristics = []
         , abilities = []
         , arts = []
         , spells = []
         , ptraits = []
         , reputations = []
         , confidence = Confidence { cscore = 1, cpoints = 3 }
         , otherTraits = []
       }  


instance HasTime CharacterSheet where
    timeOf = csTime

