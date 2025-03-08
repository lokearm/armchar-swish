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
                          , advanceCharacter
                          ) where

import GHC.Generics
import Data.Aeson
import Data.Maybe (fromMaybe,fromJust,isNothing) -- ,isJust)
-- import Data.Aeson.Types (Parser)

import ArM.Char.Trait
import ArM.Char.Advancement
import ArM.Char.Validation
import ArM.Char.Internal.KeyPair
-- import ArM.Debug.Trace
-- import ArM.Types.Season
import ArM.Helper




-- |
-- = CharacterConcept

data CharacterType = Magus | Companion | Grog
       deriving (Eq,Generic)
instance ToJSON CharacterType
instance FromJSON CharacterType

data CharacterConcept = CharacterConcept 
         { name :: String
         , charType :: CharacterType
         , house :: Maybe String
         , charGlance :: KeyPairList
         , charData :: KeyPairList
       }  deriving (Eq,Generic)

-- | Default (empty) character concept object.
defaultConcept :: CharacterConcept 
defaultConcept = CharacterConcept { name = "John Doe"
                                  , charType = Magus
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
        <*> v .: "charType"
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
        <*> fmap maybeList ( v .:? "traits" )

-- |
-- = Character

data Character = Character 
         { charID :: String
         , concept :: CharacterConcept
         , state :: Maybe CharacterState
         , pregameDesign :: [ AugmentedAdvancement ]
         , pregameAdvancement :: [ Advancement ]
         , pastAdvancement :: [ AugmentedAdvancement ]
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
        <*> fmap maybeList ( v .:? "pregameDesign" )
        <*> fmap maybeList ( v .:? "pregameAdvancement" )
        <*> fmap maybeList ( v .:? "pastAdvancement" )
        <*> fmap maybeList ( v .:? "futureAdvancement" )

-- |
-- = Advancement



-- |
-- == Char Gen Advancement



-- | Augment and amend the advancements based on current virtues and flaws.
prepareCharGen :: [VF] -> Advancement -> AugmentedAdvancement
prepareCharGen vfs = validate . initialLimits vfs . addInferredTraits 


-- | Compute the initial state if no state is recorded.
prepareCharacter :: Character -> Character
prepareCharacter c 
            | state c /= Nothing = c
            | otherwise = c { state = Just $ cs { charTime = Just "Game start" }
                            , pregameDesign = xs
                            , pregameAdvancement = []
                            }
            where as = pregameAdvancement  c 
                  (xs,cs) = applyCGA vfs as defaultCS
                  vfs = getInitVF $ head as

getInitVF :: Advancement -> [ VF ]
getInitVF a | m /= "Virtues and Flaws" = []
            | otherwise = getVF $ changes a
           where m = fromMaybe "" $ mode a


-- | Apply CharGen advancement
applyCharGenAdv :: [VF] -> Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyCharGenAdv vfs a cs = (a',cs')
    where a' = prepareCharGen vfs a
          cs' = cs { charTime = season a, traits = new }
          new =  advance change tmp
          tmp =  advance inferred old 
          change = sortTraits $ changes a'
          inferred = inferredTraits a'
          old = traits cs

-- | Apply a list of advancements
applyCGA :: [VF] -> [Advancement] -> CharacterState -> ([AugmentedAdvancement],CharacterState)
applyCGA vfs a cs = applyCGA' vfs ([],a,cs)
applyCGA' :: [VF] -> ([AugmentedAdvancement],[Advancement],CharacterState)
                   -> ([AugmentedAdvancement],CharacterState)
applyCGA' _ (xs,[],cs) = (xs,cs)
applyCGA' vfs (xs,y:ys,cs) = applyCGA' vfs (a':xs,ys,cs')
    where (a',cs') = applyCharGenAdv vfs y cs

-- |
-- == In Game Advancement


-- | Apply advancement
applyAdvancement :: Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyAdvancement a cs = (a',cs')
    where a' = prepareAdvancement cs a
          cs' = cs { charTime = season a, traits = new }
          new =  advance change tmp
          tmp =  advance inferred old 
          change = sortTraits $ changes a'
          inferred = inferredTraits a'
          old = traits cs

-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: CharacterState -> Advancement -> AugmentedAdvancement
prepareAdvancement _ = validate . addInferredTraits

{-
-- | Apply a list of advancements
applyAdvancements :: [Advancement] -> CharacterState -> ([AugmentedAdvancement],CharacterState)
applyAdvancements a cs = applyAdvancements' ([],a,cs)
applyAdvancements' :: ([AugmentedAdvancement],[Advancement],CharacterState)
                   -> ([AugmentedAdvancement],CharacterState)
applyAdvancements' (xs,[],cs) = (xs,cs)
applyAdvancements' (xs,y:ys,cs) = applyAdvancements' (a':xs,ys,cs')
    where (a',cs') = applyAdvancement y cs

-}


-- | Advance the character until after the given time.
advanceCharacter :: CharTime -> Character -> Character
advanceCharacter ct c | isNothing (state c) = advanceCharacter ct $ prepareCharacter c
                      | ct > ct' = c
                      | otherwise = stepCharacter c 
            where y = head $ futureAdvancement c
                  ct' = season y

-- | Advance the character one season forward
stepCharacter :: Character -> Character
stepCharacter c = c { state = Just cs 
                            , pastAdvancement = (a:xs)
                            , futureAdvancement = ys 
                            }
            where y = head $ futureAdvancement c
                  ys = tail $ futureAdvancement c
                  xs = pastAdvancement c
                  (a,cs) = applyAdvancement y (fromJust $ state c)
