{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Char.Character ( Character(..)
                          , defaultCharacter
                          , CharacterConcept(..)
                          , defaultConcept
                          , CharacterState(..)
                          , KeyPairList(..)
                          , KeyPair(..)
                          , FieldValue(..)
                          , prepareCharacter
                          , Advancement(..)
                          , computeCS
                          , fullName
                          , fullConceptName
                          ) where

import GHC.Generics
import Data.Maybe (fromJust,isNothing)
import Data.Aeson
-- import Data.Aeson.Types (Parser)

import ArM.Char.Trait
import ArM.Char.Internal.KeyPair
-- import ArM.Types.Season

type CharTime = Maybe String

listNothing :: Maybe [a] -> [a]
listNothing Nothing = []
listNothing (Just xs) = xs


-- = CharacterConcept

data CharacterConcept = CharacterConcept 
         { name :: String
         , house :: Maybe String
         , charGlance :: KeyPairList
         , charData :: KeyPairList
       }  deriving (Eq,Generic)

defaultConcept :: CharacterConcept 
defaultConcept = CharacterConcept { name = "John Doe"
                                  , house = Nothing
                                  , charGlance = KeyPairList []
                                  , charData = KeyPairList []
       }  

instance ToJSON CharacterConcept where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CharacterConcept where
    parseJSON = withObject "CharacterConcept" $ \v -> CharacterConcept
        <$> v .: "name"
        <*> v .:? "house"
        <*> v .: "charGlance"
        <*> v .: "charData"

-- = CharacterState

data CharacterState = CharacterState 
         { charTime :: CharTime
         , vfList :: [ VF ]
         , abilityList :: [ Ability ]
         , artList :: [ Art ]
         , spellList :: [ Spell ]
         , reputationList :: [ Reputation ]
         , traits :: [ Trait ]
         , protoTraits :: [ ProtoTrait ]
         }  deriving (Eq,Generic)

defaultCS :: CharacterState 
defaultCS = CharacterState 
         { charTime = Nothing
         , vfList = [ ]
         , abilityList = [ ]
         , artList = [ ]
         , spellList = [ ]
         , reputationList = []
         , traits = [ ]
         , protoTraits = [ ]
         }  

instance ToJSON CharacterState where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CharacterState where
    parseJSON = withObject "CharacterState" $ \v -> CharacterState
        <$> v .:? "charTime"
        <*> fmap listNothing ( v .:? "vfList" )
        <*> fmap listNothing ( v .:? "abilityList" )
        <*> fmap listNothing ( v .:? "artList" )
        <*> fmap listNothing ( v .:? "spellList" )
        <*> fmap listNothing ( v .:? "reputationList" )
        <*> fmap listNothing ( v .:? "traits" )
        <*> fmap listNothing ( v .:? "protoTraits" )

-- = Character

data Character = Character 
         { charID :: String
         , concept :: CharacterConcept
         , state :: Maybe CharacterState
         , pregameAdvancement :: [ Advancement ]
         , pastAdvancement :: [ Advancement ]
         , futureAdvancement :: [ Advancement ]
         }  deriving (Eq,Generic)


defaultCharacter :: Character 
defaultCharacter = Character { charID = "N/A"
                             , concept = defaultConcept
                             , state = Nothing
                             , pregameAdvancement = [ ]
                             , pastAdvancement = [ ]
                             , futureAdvancement = [ ]
       }  


instance ToJSON Character where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Character where
    parseJSON = withObject "Character" $ \v -> Character
        <$> v .: "charID"
        <*> v .: "concept"
        <*> v .:? "state" 
        <*> fmap listNothing ( v .:? "pregameAdvancement" )
        <*> fmap listNothing ( v .:? "pastAdvancement" )
        <*> fmap listNothing ( v .:? "futureAdvancement" )




-- | Compute the initial state if no state is recorded.
prepareCharacter :: Character -> Character
prepareCharacter c 
            | state c /= Nothing = c
            | otherwise = c { state = Just s }
            where s = pregameBuild $ pregameAdvancement  c 

-- | Process pregameAdvancement to compute initial CharacterState
pregameBuild :: [ Advancement ] -> CharacterState
pregameBuild as = filterCS $ defaultCS { charTime = Just "Game Start"
                            , traits = computeCS bs
                            , protoTraits = bs 
                            }
    where bs = pregameAdvance [] as
filterCS :: CharacterState -> CharacterState
filterCS cs = cs { vfList = x1
                 , abilityList = x2
                 , artList = x3
                 , spellList = x4
                 , reputationList = x5
                 , traits = y5
                }
           where (x1,y1) = filterTrait $ traits cs
                 (x2,y2) = filterTrait y1
                 (x3,y3) = filterTrait y2
                 (x4,y4) = filterTrait y3
                 (x5,y5) = filterTrait y4

class TraitType t where
    filterTrait :: [ Trait ] -> ( [ t ], [ Trait ] )
    filterTrait ts = y where (_,y) = filterTrait' (ts,([],[]))
    filterTrait' :: ( [ Trait ], ( [ t ], [ Trait ] ) )
                  -> ( [ Trait ], ( [ t ], [ Trait ] ) )
    filterTrait' ([],y) = ([],y)
    filterTrait' (x:xs,(ys,zs)) | isNothing ab  = (xs,(ys,x:zs))
                                | otherwise = (xs,(fromJust ab:ys,zs))
        where ab = getTrait x
    getTrait :: Trait -> Maybe t

instance TraitType VF where
    getTrait (VFTrait x) = Just x
    getTrait _ = Nothing
instance TraitType Ability where
    getTrait (AbilityTrait x) = Just x
    getTrait _ = Nothing
instance TraitType Art where
    getTrait (ArtTrait x) = Just x
    getTrait _ = Nothing
instance TraitType Spell where
    getTrait (SpellTrait x) = Just x
    getTrait _ = Nothing
instance TraitType Reputation where
    getTrait (ReputationTrait x) = Just x
    getTrait _ = Nothing


computeCS :: [ ProtoTrait ] -> [ Trait ]
computeCS = map processTrait
pregameAdvance :: [ ProtoTrait ]  -> [ Advancement ] -> [ ProtoTrait ] 
pregameAdvance xs [] = xs
pregameAdvance xs (y:ys) = pregameAdvance ns ys
   where ns = advanceTraits (changes y) xs

-- = Advancement

data Season = Spring | Summer | Autumn | Winter 
   deriving (Show,Ord,Eq)
data AdvancementType = Practice | Exposure | Adventure 
                     | Teaching | Training | Reading | VisStudy
   deriving (Show,Ord,Eq)
data ExposureType = LabWork | Teach | Train 
                  | Writing | Copying | OtherExposure | NoExposure
   deriving (Show,Ord,Eq)

data Advancement = Advancement { mode :: Maybe String
                               , season :: CharTime
                               , narrative :: Maybe String
                               , totalXP :: Maybe Int
                               , changes :: [ ProtoTrait ]
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
        <*> v .:? "totalXP"
        <*> fmap listNothing ( v .:? "changes" )

-- = Show Instances
instance Show CharacterConcept where
   show c = fullConceptName c ++ "\n"
         ++ ( show $ charGlance c ) ++ ( show $ charData c )
instance Show Character where
   show = show . concept 

-- = Other display functions

fullConceptName :: CharacterConcept -> String
fullConceptName c = name c ++ (f $ house c)
      where f Nothing = ""
            f (Just x) | take 2 x == "ex" = " " ++ x
                       | otherwise  = " ex " ++ x
fullName :: Character -> String
fullName = fullConceptName . concept

