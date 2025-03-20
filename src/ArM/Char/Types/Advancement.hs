{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Types.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  The Advancement types representing changes over a season.
--
-----------------------------------------------------------------------------
module ArM.Char.Types.Advancement where

import ArM.Helper
import ArM.Char.Trait
import ArM.GameRules

import Data.Aeson
import GHC.Generics
import Data.List.Split
import Text.Read


-- type CharTime = Maybe String

-- | Season of the year.
-- This is currently defined using the Hibernian calendar,
-- with Winter being the first season of the year.
-- This must be changed for the standard calendare.
-- data Season = Spring | Summer | Autumn | Winter | NoSeason
data Season = Winter | Spring | Summer | Autumn  | NoSeason
     deriving (Show,Ord,Eq,Read,Generic)

instance ToJSON SeasonTime where
   toJSON = toJSON . show
instance FromJSON SeasonTime 
instance ToJSON Season
instance FromJSON Season
{-
instance FromJSON YesNo where

    -- parseJSON takes a Value, it could be one of follwing data constructors:
    -- Object, Array, String, Number, Bool or Null.
    -- First of all we expect an Object, it is defined as Object !Object,
    -- where second Object is just a type synonym for HashMap Text Value. In
    -- our case we should choose somehow our Haskell value constructor
    -- according to recieved value.
    -- So, `o` is actually a HashMap, and all we need is to lookup key "type"
    -- We should use strict Text for key:
    parseJSON (Object o) = case HML.lookup (pack "type") o of
        -- value of entity has type Value
        Just (String t) -> fromString (TL.unpack (TL.fromStrict t))
        Just (Number n) -> fromNum n
        -- Other cases are invalid
        _               -> empty
        where fromString :: String -> Parser YesNo
              fromString "yes" = pure Yes
              fromString "no"  = pure No
              fromString _     = empty
              fromNum n
                  | n == 1 || n == 1.0 = pure Yes
                  | n == 0 || n == 0.0 = pure No
                  | otherwise = empty
-}
data SeasonTime = SeasonTime Season Int | GameStart | NoTime deriving (Eq,Generic)
isWinter :: SeasonTime -> Bool
isWinter (SeasonTime Winter _) = True
isWinter _ = False

parseSeasonTime :: Maybe String -> SeasonTime
parseSeasonTime Nothing = NoTime
parseSeasonTime (Just "GameStart") = GameStart
parseSeasonTime (Just "Game Start") = GameStart
parseSeasonTime (Just s) = fy ys
    where xs = splitOn " " s
          ys = map readMaybe xs :: [Maybe Int]
          ss = map readMaybe xs :: [Maybe Season]
          fs [] = NoSeason
          fs (Nothing:rest) = fs rest
          fs (Just r:_) = r
          st = fs ss
          fy [] = NoTime
          fy (Nothing:rest) = fy rest
          fy (Just r:_) = SeasonTime st r


instance Show SeasonTime where
   show GameStart = "Game Start"
   show (SeasonTime s y) = show s ++ " " ++ show y
   show NoTime =  "N/A"

instance Ord SeasonTime where
    (<=) NoTime _ = False
    (<=) _ NoTime = True
    (<=) GameStart _ = True
    (<=) _ GameStart = False
    (<=) (SeasonTime s1 y1) (SeasonTime s2 y2) 
        | y1 == y2 = s1 <= s2
        | otherwise = y1 <= y2




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
     season :: a -> SeasonTime    -- ^ season or development stage
     narrative :: a -> Maybe String -- ^ freeform description of the activities
     uses :: a -> Maybe [ Resource ] -- ^ Books and other resources used exclusively by the character
     sourceQuality :: a -> Maybe XPType -- ^ Source Quality (SQ)
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
     , advSeason :: SeasonTime    -- ^ season or development stage
     , advYears :: Maybe Int    -- ^ number of years advanced
     , advNarrative :: Maybe String -- ^ freeform description of the activities
     , advUses :: Maybe [ Resource ] -- ^ Books and other resources used exclusively by the character
     , advSQ :: Maybe XPType -- ^ Source Quality (SQ) This should be the common SQ for adventures; individual variation should be recorded as `advBonus`
     , advBonus :: Maybe XPType -- ^ Bonus to Source Quality (SQ)
     , advChanges :: [ ProtoTrait ]  -- ^ trait changes defined by player
     }
   deriving (Eq,Generic,Show)

defaultAdv :: Advancement 
defaultAdv = Advancement 
     { advMode = Nothing
     , advSeason = NoTime
     , advYears = Nothing
     , advNarrative = Nothing
     , advUses = Nothing
     , advSQ = Nothing
     , advBonus = Nothing
     , advChanges = [ ]  
     }

data Validation = ValidationError String | Validated String
   deriving (Eq,Generic)

instance Show Validation where
    show (ValidationError x) = "ERROR: " ++ x
    show (Validated x) = "Validated: " ++ x

-- | Advancement with additional inferred fields
data AugmentedAdvancement = Adv
     { advancement :: Advancement -- ^ Base advancement as entered by the user
     , effectiveSQ :: Maybe XPType   -- ^ SQ modified by virtues and flaws
     , levelLimit :: Maybe Int   -- ^ spell level allowance
     , spentXP  :: Maybe XPType   -- ^ Total XP spent on advancement
     , inferredTraits :: [ ProtoTrait ] -- ^ trait changes inferred by virtues and flaws
     , augYears :: Maybe Int    -- ^ number of years advanced
     , validation :: [Validation] -- ^ Report from validation
     }
   deriving (Eq,Generic,Show)


defaultAA :: AugmentedAdvancement
defaultAA = Adv
     { advancement = defaultAdv
     , effectiveSQ = Nothing
     , levelLimit = Nothing 
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
     sourceQuality  = advSQ
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
        -- <*> v .:? "season"
        <*> fmap parseSeasonTime ( v .:? "season" )
        <*> v .:? "years"
        <*> v .:? "narrative"
        <*> v .:? "uses"
        <*> v .:? "sourceQuality"
        <*> v .:? "bonusQuality"
        <*> fmap maybeList ( v .:? "changes" )
instance FromJSON AugmentedAdvancement where
    parseJSON = withObject "AugmentedAdvancement" $ \v -> Adv
        <$> v .: "advancement"
        <*> v .:? "effectiveSQ"
        <*> v .:? "levels"
        <*> v .:? "spentXP"
        <*> fmap maybeList ( v .:? "inferredTraits" )
        <*> v .:? "augYears"
        <*> fmap maybeList ( v .:?  "validation")

