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
import Data.Maybe (fromJust,isJust,fromMaybe)
import GHC.Generics

type CharTime = Maybe String

data Season = Spring | Summer | Autumn | Winter 
     deriving (Show,Ord,Eq,Read)

data SeasonTime = SeasonTime Season Int | GameStart deriving (Eq)

instance Show SeasonTime where
   show GameStart = "Game Start"
   show (SeasonTime s y) = show s ++ " " ++ show y

instance Ord SeasonTime where
    compare GameStart (SeasonTime _ _) = LT
    compare (SeasonTime _ _) GameStart = GT
    compare GameStart GameStart = EQ
    compare (SeasonTime s1 y1) (SeasonTime s2 y2) 
        | y1 < y2 = LT
        | y1 > y2 = GT
        | otherwise = compare s1 s2 




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
     , advYears :: Maybe Int    -- ^ number of years advanced
     , advNarrative :: Maybe String -- ^ freeform description of the activities
     , advUses :: Maybe [ Resource ] -- ^ Books and other resources used exclusively by the character
     , advSQ :: Maybe Int -- ^ Source Quality (SQ)
     , advChanges :: [ ProtoTrait ]  -- ^ trait changes defined by player
     }
   deriving (Eq,Generic,Show)

defaultAdv :: Advancement 
defaultAdv = Advancement 
     { advMode = Nothing
     , advSeason = Nothing
     , advYears = Nothing
     , advNarrative = Nothing
     , advUses = Nothing
     , advSQ = Nothing
     , advChanges = [ ]  
     }

data Validation = Error String | Validated String
   deriving (Eq,Generic,Show)
-- | Advancement with additional inferred fields
data AugmentedAdvancement = Adv
     { advancement :: Advancement -- ^ Base advancement as entered by the user
     , effectiveSQ :: Maybe Int   -- ^ SQ modified by virtues and flaws
     , spentXP  :: Maybe Int   -- ^ Total XP spent on advancement
     , inferredTraits :: [ ProtoTrait ] -- ^ trait changes inferred by virtues and flaws
     , augYears :: Maybe Int    -- ^ number of years advanced
     , validation :: [Validation] -- ^ Report from validation
     }
   deriving (Eq,Generic,Show)


defaultAA :: AugmentedAdvancement
defaultAA = Adv
     { advancement = defaultAdv
     , effectiveSQ = Nothing
     , spentXP = Nothing
     , inferredTraits = [ ] 
     , augYears = Nothing
     , validation = []
     }

instance AdvancementLike Advancement where
     mode = advMode
     season  = advSeason
     narrative  = advNarrative
     uses  = advUses
     sourceQuality  =  advSQ
     changes = advChanges
instance AdvancementLike AugmentedAdvancement where
     mode a = advMode  $ advancement a
     season  = advSeason  .  advancement 
     narrative  a = advNarrative  $ advancement  a
     uses  a = advUses  $ advancement a
     sourceQuality  a =  advSQ  $ advancement a
     changes  a = advChanges  $ advancement  a

instance ToJSON Validation
instance FromJSON Validation

instance ToJSON AugmentedAdvancement where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Advancement where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Advancement where
    parseJSON = withObject "Advancement" $ \v -> Advancement
        <$> v .:? "mode"
        <*> v .:? "season"
        <*> v .:? "years"
        <*> v .:? "narrative"
        <*> v .:? "uses"
        <*> v .:? "sourceQuality"
        <*> fmap maybeList ( v .:? "changes" )
instance FromJSON AugmentedAdvancement where
    parseJSON = withObject "AugmentedAdvancement" $ \v -> Adv
        <$> v .: "advancement"
        <*> v .:? "effectiveSQ"
        <*> v .:? "spentXP"
        <*> fmap maybeList ( v .:? "inferredTraits" )
        <*> v .:? "augYears"
        <*> fmap maybeList ( v .:?  "validation")


-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancementVF :: Advancement -> AugmentedAdvancement
prepareAdvancementVF a = defaultAA { inferredTraits = f a, advancement = a }
     where f = inferTraits . getVF . changes 

augmentTotalXP :: AugmentedAdvancement -> AugmentedAdvancement
augmentTotalXP a | m == "Early Childhood" = a { effectiveSQ = Just 45 }
                 | m == "Apprenticeship" = a { effectiveSQ = Just 240 }
                 | m == "Later Life" = a { effectiveSQ = Just $ y*15 }
                 | otherwise = a { effectiveSQ = Just 0 }
           where m = fromMaybe "" $ mode a
                 y = fromMaybe 0 $ advYears $ advancement a

-- | Get the virtues and flaws from a list of ProtoTrait objects, and convert them to
-- VF objects
getVF :: [ ProtoTrait ] -> [ VF ]
getVF [] = []
getVF (p:ps) | isJust (virtue p) = g p:getVF ps
             | isJust (flaw p) = g p:getVF ps
             | otherwise = getVF ps
    where g = fromJust . computeTrait
