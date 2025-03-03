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
-- Description :  Types to represent Characters and functions for advancement.
--
-- This module contains types to process characters, including 
-- persistence in JSON and advancement.
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
                          , fullName
                          , fullConceptName
                          ) where

import GHC.Generics
import Data.Aeson
-- import Data.Maybe (fromJust)
import qualified Data.Map as M
-- import Data.Aeson.Types (Parser)

import ArM.Char.Trait
import ArM.Char.Internal.KeyPair
import ArM.Char.Virtues
-- import ArM.Types.Season

type CharTime = Maybe String

listNothing :: Maybe [a] -> [a]
listNothing Nothing = []
listNothing (Just xs) = xs

filterNothing :: [Maybe a] -> [a]
filterNothing = f
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs

-- |
-- = CharacterConcept

data CharacterConcept = CharacterConcept 
         { name :: String
         , house :: Maybe String
         , charGlance :: KeyPairList
         , charData :: KeyPairList
       }  deriving (Eq,Generic)

-- | Default (empty) character concept object.
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

instance Show CharacterConcept where
   show c = fullConceptName c ++ "\n"
         ++ ( show $ charGlance c ) ++ ( show $ charData c )

-- | Return the name of the character as a string, including house affiliation
-- if defined.
fullConceptName :: CharacterConcept -> String
fullConceptName c = name c ++ (f $ house c)
      where f Nothing = ""
            f (Just x) | take 2 x == "ex" = " " ++ x
                       | otherwise  = " ex " ++ x


-- |
-- = CharacterState

data CharacterState = CharacterState 
         { charTime :: CharTime
         , traits :: [ Trait ]
         }  deriving (Eq,Generic)

-- | Default (empty) character state object.
defaultCS :: CharacterState 
defaultCS = CharacterState 
         { charTime = Nothing
         , traits = [ ]
         }  

instance ToJSON CharacterState where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CharacterState where
    parseJSON = withObject "CharacterState" $ \v -> CharacterState
        <$> v .:? "charTime"
        <*> fmap listNothing ( v .:? "traits" )

-- |
-- = Character

data Character = Character 
         { charID :: String
         , concept :: CharacterConcept
         , state :: Maybe CharacterState
         , pregameDesign :: [ Advancement ]
         , pregameAdvancement :: [ Advancement ]
         , pastAdvancement :: [ Advancement ]
         , futureAdvancement :: [ Advancement ]
         }  deriving (Eq,Generic)


-- | Default (empty) character object.
defaultCharacter :: Character 
defaultCharacter = Character { charID = "N/A"
                             , concept = defaultConcept
                             , state = Nothing
                             , pregameDesign = [ ]
                             , pregameAdvancement = [ ]
                             , pastAdvancement = [ ]
                             , futureAdvancement = [ ]
       }  

instance Show Character where
   show = show . concept 

-- | Return the name of the character as a string, including house affiliation
-- if defined.
fullName :: Character -> String
fullName = fullConceptName . concept

instance ToJSON Character where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Character where
    parseJSON = withObject "Character" $ \v -> Character
        <$> v .: "charID"
        <*> v .: "concept"
        <*> v .:? "state" 
        <*> fmap listNothing ( v .:? "pregameDesign" )
        <*> fmap listNothing ( v .:? "pregameAdvancement" )
        <*> fmap listNothing ( v .:? "pastAdvancement" )
        <*> fmap listNothing ( v .:? "futureAdvancement" )

-- |
-- = Advancement


-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: CharacterState -> Advancement -> Advancement
prepareAdvancement = prepareAdvancementVF . fst . filterTrait . traits 

-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancementVF :: [VF] -> Advancement -> Advancement
prepareAdvancementVF vfs a = a { inferredTraits = inferTraitsVF vfs }

-- | Add ProtoTrait objects infered by current virtues and flaws
inferTraitsVF :: [VF] -> [ProtoTrait]
inferTraitsVF vfs = rs
    where vf = [ M.lookup (traitKey x) virtueMap | x <- vfs ]
          app Nothing _ = Nothing
          app (Just f) x = Just $ f x
          rs = filterNothing [ app g x | (g,x) <- zip vf vfs ]

-- | Apply advancement
applyAdvancement :: Advancement -> CharacterState -> (Advancement,CharacterState)
applyAdvancement a cs = (a',cs')
    where a' = prepareAdvancement cs a
          cs' = cs { charTime = season a, traits = new }
          new = advance change $ advance inferred old 
          change = changes a'
          inferred = inferredTraits a'
          old = traits cs

-- | Apply a list of advancements
applyAdvancements :: [Advancement] -> CharacterState -> ([(Advancement,Advancement)],CharacterState)
applyAdvancements a cs = applyAdvancements' ([],a,cs)
applyAdvancements' :: ([(Advancement,Advancement)],[Advancement],CharacterState)
                   -> ([(Advancement,Advancement)],CharacterState)
applyAdvancements' (xs,[],cs) = (xs,cs)
applyAdvancements' (xs,y:ys,cs) = applyAdvancements' ((a',y):xs,ys,cs')
    where (a',cs') = applyAdvancement y cs


-- | Compute the initial state if no state is recorded.
prepareCharacter :: Character -> Character
prepareCharacter c 
            | state c /= Nothing = c
            | otherwise = c { state = Just cs
                            , pregameDesign = fst $ unzip xs
                            }
            where as = pregameAdvancement  c 
                  (xs,cs) = applyAdvancements as defaultCS

{-
-- | Process pregameAdvancement to compute initial CharacterState
pregameBuild :: [ Advancement ] -> CharacterState
pregameBuild as = defaultCS { charTime = Just "Game Start"
                            , traits = map toTrait $ pregameAdvance [] as
                            }


-- | Recursive helper for pregameBuild
pregameAdvance :: [ ProtoTrait ]  -> [ Advancement ] -> [ ProtoTrait ] 
pregameAdvance xs [] = xs
pregameAdvance xs (y:ys) = pregameAdvance ns ys
   where ns = advanceTraits (changes y) xs
-}

data Season = Spring | Summer | Autumn | Winter 
   deriving (Show,Ord,Eq)
data AdvancementType = Practice | Exposure | Adventure 
                     | Teaching | Training | Reading | VisStudy
   deriving (Show,Ord,Eq)
data ExposureType = LabWork | Teach | Train 
                  | Writing | Copying | OtherExposure | NoExposure
   deriving (Show,Ord,Eq)

-- | The advancement object has two roles.
-- It can hold the advancemet from one season or chargen stage,
-- as specified by the user.
-- It can also hold additional field inferred by virtues and flaws.
-- One may consider splitting these two functions into two types.
data Advancement = Advancement 
     { mode :: Maybe String  -- ^ mode of study
     , season :: CharTime    -- ^ season or development stage
     , narrative :: Maybe String -- ^ freeform description of the activities
     , sourceQuality :: Maybe Int -- ^ Source Quality (SQ)
     , effectiveSQ :: Maybe Int   -- ^ SQ modified by virtues and flaws
     , changes :: [ ProtoTrait ]  -- ^ trait changes defined by player
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
        <*> fmap listNothing ( v .:? "changes" )
        <*> fmap listNothing ( v .:? "inferredTraits" )

