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
import Data.Maybe (fromJust,isJust,isNothing)
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

data Resource = Resource String
   deriving (Eq,Show,Ord,Generic)
instance ToJSON Resource
instance FromJSON Resource

class AdvancementLike a where
     mode :: a -> Maybe String  -- ^ mode of study
     season :: a -> CharTime    -- ^ season or development stage
     narrative :: a -> Maybe String -- ^ freeform description of the activities
     uses :: a -> Maybe [ Resource ] -- ^ Books and other resources used exclusively by the character
     sourceQuality :: a -> Maybe Int -- ^ Source Quality (SQ)
     -- effectiveSQ :: Maybe Int   -- ^ SQ modified by virtues and flaws
     changes :: a -> [ ProtoTrait ]  -- ^ trait changes defined by player
     -- inferredTraits :: [ ProtoTrait ] -- ^ trait changes inferred by virtues and flaws

-- | The advancement object has two roles.
-- It can hold the advancemet from one season or chargen stage,
-- as specified by the user.
-- It can also hold additional field inferred by virtues and flaws.
-- One may consider splitting these two functions into two types.
data Advancement = Advancement 
     { advMode :: Maybe String  -- ^ mode of study
     , advSeason :: CharTime    -- ^ season or development stage
     , advNarrative :: Maybe String -- ^ freeform description of the activities
     , advUses :: Maybe [ Resource ] -- ^ Books and other resources used exclusively by the character
     , advSQ :: Maybe Int -- ^ Source Quality (SQ)
     , advChanges :: [ ProtoTrait ]  -- ^ trait changes defined by player
         -- ^ trait changes inferred by virtues and flaws
     }
   deriving (Eq,Generic,Show)

-- | Advancement with additional inferred fields
data AugmentedAdvancement = Adv
     { advancement :: Maybe Advancement
     , effectiveSQ :: Maybe Int   -- ^ SQ modified by virtues and flaws
     , inferredTraits :: [ ProtoTrait ] 
         -- ^ trait changes inferred by virtues and flaws
     }
   deriving (Eq,Generic,Show)

defaultAA :: AugmentedAdvancement
defaultAA = Adv
     { advancement = Nothing
     , effectiveSQ = Nothing
     , inferredTraits = [ ] 
     }

instance AdvancementLike Advancement where
     mode = advMode
     season  = advSeason
     narrative  = advNarrative
     uses  = advUses
     sourceQuality  =  advSQ
     changes = advChanges
instance AdvancementLike AugmentedAdvancement where
     mode a | isNothing (advancement a) = Nothing
            | otherwise = advMode $ fromJust $ advancement a
     season  a | isNothing (advancement a) = Nothing
          | otherwise = advSeason $ fromJust $  advancement  a
     narrative  a | isNothing (advancement a) = Nothing
          | otherwise = advNarrative $ fromJust $ advancement  a
     uses  a | isNothing (advancement a) = Nothing
          | otherwise = advUses $ fromJust $ advancement a
     sourceQuality  a | isNothing (advancement a) = Nothing
                      | otherwise =  advSQ $ fromJust $ advancement a
     changes  a | isNothing (advancement a) = []
             | otherwise = advChanges $ fromJust $ advancement  a

instance ToJSON Advancement where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Advancement where
    parseJSON = withObject "Advancement" $ \v -> Advancement
        <$> v .:? "mode"
        <*> v .:? "season"
        <*> v .:? "narrative"
        <*> v .:? "uses"
        <*> v .:? "sourceQuality"
        <*> fmap maybeList ( v .:? "changes" )
instance FromJSON AugmentedAdvancement where
    parseJSON = withObject "AugmentedAdvancement" $ \v -> Adv
        <$> v .:? "advancement"
        <*> v .:? "effectiveSQ"
        <*> fmap maybeList ( v .:? "inferredTraits" )


-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancementVF :: Advancement -> AugmentedAdvancement
prepareAdvancementVF a = defaultAA { inferredTraits = f a, advancement = Just a }
     where f = inferTraits . getVF . changes 

-- | Get the virtues and flaws from a list of ProtoTrait objects, and convert them to
-- VF objects
getVF :: [ ProtoTrait ] -> [ VF ]
getVF [] = []
getVF (p:ps) | isJust (virtue p) = g p:getVF ps
             | isJust (flaw p) = g p:getVF ps
             | otherwise = getVF ps
    where g = fromJust . computeTrait
