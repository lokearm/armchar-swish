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
import ArM.Types.Advancement
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
         , pastCovAdvancement :: [ AugmentedAdvancement ]
         , futureCovAdvancement :: [ Advancement ]
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

augHead :: SeasonTime -> Maybe String -> String
augHead NoTime Nothing = ("??" )
augHead x Nothing = (show x )
augHead NoTime (Just x) = x
augHead x (Just z) = (show x  ++ " " ++ z)

pregameCharErrors :: Character -> [(String,[String])]
pregameCharErrors c = renderCharErrors c $ pregameDesign c

renderCharErrors :: Character -> [AugmentedAdvancement] -> [(String,[String])]
renderCharErrors c as = ff $ map f as
   where f a = (charID c ++ ": " ++ augHead (season a) (mode a)
               , filterError $ validation a)
         ff ((_,[]):xs) = ff xs
         ff (x:xs) = x:ff xs
         ff [] = []

ingameCharErrors :: Character -> [(String,[String])]
ingameCharErrors c = renderCharErrors c $ pastAdvancement c

filterError :: [Validation] -> [String]
filterError (Validated _:xs) = filterError xs
filterError (ValidationError x:xs) = x:filterError xs
filterError [] = []

-- | Exctract a list of validation errors 
pregameErrors :: Saga -> [String]
pregameErrors saga = foldl (++) [] $ map formatOutput vvs
    where cs = gameStartCharacters saga
          vs = map pregameCharErrors  cs
          vvs = foldl (++) [] vs
          formatOutput (x,xs) = ("+ " ++ x):map ("    + "++) xs

-- | Exctract a list of validation errors 
ingameErrors :: Saga -> [String]
ingameErrors saga = foldl (++) [] $ map formatOutput vvs
    where cs = currentCharacters saga
          vs = map ingameCharErrors  cs
          vvs = foldl (++) [] vs
          formatOutput (x,xs) = ("+ " ++ x):map ("    + "++) xs
