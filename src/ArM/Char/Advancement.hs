{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  The Advancement types representing changes over a season.
--
-----------------------------------------------------------------------------
module ArM.Char.Advancement where

-- import ArM.Char.Character
import ArM.Helper
import ArM.Char.Trait
import ArM.Char.Virtues

import Data.Aeson
import Data.Maybe (fromJust,isJust)
import GHC.Generics

type CharTime = Maybe String


data Season = Spring | Summer | Autumn | Winter 
   deriving (Show,Ord,Eq)
data AdvancementType = Practice | Exposure | Adventure 
                     | Teaching | Training | Reading | VisStudy
   deriving (Show,Ord,Eq)
data ExposureType = LabWork | Teach | Train 
                  | Writing | Copying | OtherExposure | NoExposure
   deriving (Show,Ord,Eq)

data Resource = Resource ""

-- | The advancement object has two roles.
-- It can hold the advancemet from one season or chargen stage,
-- as specified by the user.
-- It can also hold additional field inferred by virtues and flaws.
-- One may consider splitting these two functions into two types.
data Advancement = Advancement 
     { mode :: Maybe String  -- ^ mode of study
     , season :: CharTime    -- ^ season or development stage
     , narrative :: Maybe String -- ^ freeform description of the activities
     , uses :: Maybe [ Resource ] -- ^ Books and other resources used exclusively by the character
     , sourceQuality :: Maybe Int -- ^ Source Quality (SQ)
     , effectiveSQ :: Maybe Int   -- ^ SQ modified by virtues and flaws
     , changes :: [ ProtoTrait ]  -- ^ trait changes defined by player
     , inferredTraits :: [ ProtoTrait ] 
         -- ^ trait changes inferred by virtues and flaws
     }
   deriving (Eq,Generic,Show)

-- | Advancement with additional inferred fields
data AugmentedAdvancement = Adv
     { effectiveSQ :: Maybe Int   -- ^ SQ modified by virtues and flaws
     , inferredTraits :: [ ProtoTrait ] 
         -- ^ trait changes inferred by virtues and flaws
     }
   deriving (Eq,Generic,Show)

instance ToJSON Advancement where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Advancement where
    parseJSON = withObject "Advancement" $ \v -> Advancement
        <$> v .:? "mode"
        <*> v .:? "season"
        <*> v .:? "narrative"
        <*> v .:? "sourceQuality"
        <*> v .:? "effectiveSQ"
        <*> fmap maybeList ( v .:? "changes" )
        <*> fmap maybeList ( v .:? "inferredTraits" )


-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancementVF :: Advancement -> Advancement
prepareAdvancementVF a = a { inferredTraits = f a }
     where f = inferTraits . getVF . changes 

-- | Get the virtues and flaws from a list of ProtoTrait objects, and convert them to
-- VF objects
getVF :: [ ProtoTrait ] -> [ VF ]
getVF [] = []
getVF (p:ps) | isJust (virtue p) = g p:getVF ps
             | isJust (flaw p) = g p:getVF ps
             | otherwise = getVF ps
    where g = fromJust . computeTrait
