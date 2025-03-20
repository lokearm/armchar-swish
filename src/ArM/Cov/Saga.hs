{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Cov.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Saga type with references to constituent files and objects.
--
--
-----------------------------------------------------------------------------
module ArM.Cov.Saga where

-- import Data.Maybe 
import Data.Aeson 
import GHC.Generics

-- import ArM.Char.Trait
import ArM.Char.Character
import ArM.Char.Types.Advancement
import ArM.Char.Spell
import ArM.Char.Markdown
import ArM.Cov.Covenant

-- import ArM.Debug.Trace
--

-- |
-- = Saga objects

-- | A Saga as it is processed in memory.
-- Multiple files have to be loaded to generate a Saga object from a `SagaFile`.
data Saga = Saga 
         { sagaTitle :: String
         , covenants :: [Covenant]
         , currentDir :: Maybe String
         , gamestartDir :: Maybe String
         , gameStartCharacters :: [Character]
         , currentCharacters :: [Character]
         , spells :: SpellDB
       }  deriving (Eq,Show)

advanceSaga' :: SeasonTime -> Saga -> Saga
advanceSaga' t saga = saga { currentCharacters = map (advanceCharacter t) ( gameStartCharacters saga ) }

advanceSaga :: SagaFile -> Saga -> Saga
advanceSaga t saga = advanceSaga' (currentSeason t) saga

-- | A Saga as it is stored on file.
-- The main purpose here is to identify all the files used for characters and
-- other data in the saga.
data SagaFile = SagaFile 
         { title :: String
	 , currentSeason :: SeasonTime
         , currentDirectory :: Maybe String
         , gamestartDirectory :: Maybe String
         , covenantFiles :: [String]
         , characterFiles :: [String]
         , spellFile :: String
       }  deriving (Eq,Generic,Show)

instance ToJSON SagaFile 
instance FromJSON SagaFile 

instance Markdown Saga where
    printMD saga = OList 
        [ OString $ "# " ++ sagaTitle saga
        ]

-- |
-- = Advancement

-- |
-- = Error reports


-- | Get errors from the ingam advancements of a given character.
pregameCharErrors :: Character -> [(String,[String])]
pregameCharErrors c = renderCharErrors c $ pregameDesign c

-- | Get errors from the ingam advancements of a given character.
ingameCharErrors :: Character -> [(String,[String])]
ingameCharErrors c = renderCharErrors c $ pastAdvancement c

-- | Format strins for `pregameCharErrors` and `ingameCharErrors`
renderCharErrors :: Character -> [AugmentedAdvancement] -> [(String,[String])]
renderCharErrors c as = ff $ map f as
   where f a = (charID c ++ ": " ++ augHead (season a) (mode a)
               , filterError $ validation a)
         ff ((_,[]):xs) = ff xs
         ff (x:xs) = x:ff xs
         ff [] = []

-- | Format a header for `rencerCharErrors`
augHead :: SeasonTime -> Maybe String -> String
augHead NoTime Nothing = ("??" )
augHead x Nothing = (show x )
augHead NoTime (Just x) = x
augHead x (Just z) = (show x  ++ " " ++ z)

-- | Get errors from a list of Validation objects
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
