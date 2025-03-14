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

import Data.Maybe 
import Data.Aeson 
import GHC.Generics

import ArM.Char.IO
import ArM.Char.Trait
import ArM.Char.Internal.Character
import ArM.Char.Internal.Advancement
import ArM.Char.Spell

-- import ArM.Debug.Trace
--

loadSaga :: SagaFile -> IO Saga
loadSaga saga = do
   db <- readSpellDB $ spellFile saga
   return Saga { covenants = []  
           , characters = []
           , spells = fromJust db }

data Saga = Saga 
         { covenants :: [Covenant]
         , characters :: [Character]
         , spells :: SpellDB
       }  deriving (Eq,Show)

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
