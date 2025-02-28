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
                           , defaultCS
                           , computeCharacter
                           ) where

import qualified ArM.Char.Character as C
import qualified ArM.Char.Trait as T
import ArM.Types.Season


-- | 
-- = Character Sheet

data CharacterSheet = CharacterSheet 
         { csID :: String
         , csTime :: CharTime  -- ^ Current Year
         , charGlance :: C.KeyPairList
         , charData :: C.KeyPairList
         , vf :: [ T.Trait ]
         , characteristics :: [ T.Trait ]
         , abilities :: [ T.Trait ]
         , arts :: [ T.Trait ]
         , spells :: [ T.Trait ]
         , ptraits :: [ T.Trait ]
         , reputations :: [ T.Trait ]
         , confidence :: T.Trait 
         , otherTraits :: [ T.Trait ]
       }  deriving (Eq)
instance Show CharacterSheet where
    show cs = "**" ++ show (csID cs) ++ "**\n" 

defaultCS :: CharacterSheet
defaultCS = CharacterSheet {
         csID = "N/A"
         , csTime = defaultCharTime
         , charGlance = C.KeyPairList []
         , charData = C.KeyPairList []
         , vf = []
         , characteristics = []
         , abilities = []
         , arts = []
         , spells = []
         , ptraits = []
         , reputations = []
         , confidence = T.Confidence { T.cscore = 1, T.cpoints = 3 }
         , otherTraits = []
       }  

instance HasTime CharacterSheet where
    timeOf = csTime

computeCharacter :: C.Character -> CharacterSheet
computeCharacter _ = defaultCS

