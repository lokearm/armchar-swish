{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Saga type with references to constituent files and objects.
--
--
-----------------------------------------------------------------------------
module ArM.Char.Saga where

-- import Data.Maybe 
import Data.Aeson 
import GHC.Generics

import ArM.Char.Trait
import ArM.Char.Character
import ArM.Char.Internal.Advancement
import ArM.Char.Spell

-- import ArM.Debug.Trace
--


data Saga = Saga 
         { covenants :: [Covenant]
         , gameStartCharacters :: [Character]
         , currentCharacters :: [Character]
         , spells :: SpellDB
       }  deriving (Eq,Show)

advanceSaga :: SeasonTime -> Saga -> Saga
advanceSaga t saga = saga { currentCharacters = map (advanceCharacter t) ( gameStartCharacters saga ) }

data SagaFile = SagaFile 
         { covenantFiles :: [String]
         , characterFiles :: [String]
         , spellFile :: String
       }  deriving (Eq,Generic,Show)

instance ToJSON SagaFile 
instance FromJSON SagaFile 

data Covenant = Covenant 
         { covName :: String
         , covenantConcept :: KeyPairList
         , covAppearance :: Maybe String
         , founded :: Maybe Int
         , covData :: KeyPairList
         , covenantState :: CovenantState
         , pastAdvancement :: [ AugmentedAdvancement ]
         , futureAdvancement :: [ Advancement ]
       }  deriving (Eq,Generic,Show)
instance ToJSON Covenant 
instance FromJSON Covenant 

data CovenantState = CovenantState
         { library :: [Book]
         , covenfolk :: [Character]
         }  deriving (Eq,Generic,Show)
instance ToJSON CovenantState
instance FromJSON CovenantState

data Book = Book
         { title :: String
         , topic :: TraitKey
         , quality :: Int
         , bookLevel :: Maybe Int
         , author :: String
         , year :: Int
         , annotation :: String
       }  deriving (Eq,Generic,Show)
instance ToJSON Book
instance FromJSON Book

-- | Exctract a list of validation errors 
pregameErrors :: Saga -> [String]
pregameErrors saga = foldl (++) [] vs
    where f (Validated _:xs) = f xs
          f (ValidationError x:xs) = x:f xs
          f [] = []
          vs = map g (gameStartCharacters saga)
          g = foldl (++) [] . map (f . validation) . pregameDesign

