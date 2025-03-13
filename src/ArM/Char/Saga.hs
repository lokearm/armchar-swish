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
       }  deriving (Eq)

data SagaFile = SagaFile 
         { covenantFiles :: [String]
         , characterFiles :: [String]
         , spellFile :: String
       }  deriving (Eq,Generic)

data Covenant = Covenant 
         { covName :: String
         , covenantConcept :: [KeyPair]
         , covAppearance :: Maybe String
         , founded :: Maybe Int
         , covData :: KeyPairList
         , covenantState :: CovenantState
         , pastAdvancement :: [ AugmentedAdvancement ]
         , futureAdvancement :: [ Advancement ]
       }  deriving (Eq)

data CovenantState = CovenantState
         { library :: [Book]
         , covenfolk :: [Character]
         }  deriving (Eq)
data Book = Book
         { title :: String
         , topic :: TraitKey
         , quality :: Int
         , bookLevel :: Maybe Int
         , author :: String
         , year :: Int
         , annotation :: String
       }  deriving (Eq)
