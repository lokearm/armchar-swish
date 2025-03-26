{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
import ArM.DB.Spell
import ArM.DB.Weapon
import ArM.Cov.Covenant
import ArM.BasicIO
import ArM.Helper

-- import ArM.Debug.Trace

-- |
-- = Saga objects

-- | A Saga as it is processed in memory.
-- Multiple files have to be loaded to generate a Saga object from a `SagaFile`.
data Saga = Saga 
         { sagaTitle :: String
         , sagaStates :: [ SagaState ]
         , rootDir :: String
         , gameStartCharacters :: [Character]
         , gameStartCovenants :: [Covenant]
         , spells :: SpellDB
         , weapons :: WeaponDB
         , armour :: ArmourDB
       }  deriving (Eq)

-- | Get the most recent SagaState
sagaState :: Saga -> SagaState
sagaState = head . sagaStates

instance Show Saga where
   show saga = "Saga: " ++ sagaTitle saga

currentCharacters :: Saga -> [Character]
currentCharacters = characters . sagaState
currentCovenants :: Saga -> [Covenant]
currentCovenants = covenants . sagaState

data SagaState = SagaState 
         { seasonTime :: SeasonTime
         , covenants :: [Covenant]
         , characters :: [Character]
         }  deriving (Eq,Show)


-- | Get the name of the Saga
sagaStateName :: Saga -> String
sagaStateName s = sagaTitle s ++ " - " ++ (show $ seasonTime $ sagaState s)

-- | Get a string identifying the Saga State, i.e. name and season.
sagaStartName :: Saga -> String
sagaStartName s = sagaTitle s ++ " - Game Start"


-- | Directory for Game Start sheet
gamestartDir :: Saga -> String
gamestartDir saga = rootDir saga ++ "/GameStart/"

-- | Directory for Current state
currentDir :: Saga -> String
currentDir saga = rootDir saga ++ "/" ++ (show $ seasonTime $ sagaState saga) ++ "/"

-- | Make the index page for the game start description
sagaGameStartIndex :: Saga -> OList
sagaGameStartIndex saga = OList 
        [ OString $ "# " ++ sagaTitle saga ++ " - Game Start"
        , OString ""
        , characterIndex $ gameStartCharacters saga
        ]

-- | Make the index page for the current state
sagaCurrentIndex :: Saga -> OList
sagaCurrentIndex saga = OList 
        [ OString $ "# " ++ sagaTitle saga ++ " - " ++ (show $ seasonTime $ sagaState saga)
        , OString ""
        , characterIndex $ currentCharacters saga
        ]

-- |
-- == SagaFile object


-- | A Saga as it is stored on file.
-- The main purpose here is to identify all the files used for characters and
-- other data in the saga.
data SagaFile = SagaFile 
         { title :: String
         , seasons :: [ SeasonTime ]
         , currentSeason :: SeasonTime
         , rootDirectory :: Maybe String
         , covenantFiles :: [String]
         , characterFiles :: [String]
         , spellFile :: String
         , weaponFile :: String
         , armourFile :: String
       }  deriving (Eq,Generic,Show)

instance ToJSON SagaFile 
instance FromJSON SagaFile where
    parseJSON = withObject "SagaFile" $ \v -> SagaFile
       <$> v .: "title"
       <*> v .:? "seasons" .!= []
       <*> v .:? "currentSeason" .!= NoTime
       <*> v .:? "rootDirectory" 
       <*> v .:? "covenantFiles" .!= []
       <*> v .:? "characterFiles" .!= []
       <*> v .:? "spellFile" .!= "spells.csv"
       <*> v .:? "weaponFile" .!= "weapons.csv"
       <*> v .:? "armourFile" .!= "armour.csv"


-- |
-- = Advancement

advanceSagaState :: Saga -> SeasonTime -> SagaState -> SagaState
advanceSagaState saga t st = st { characters =
                             map (advance t) ( gameStartCharacters saga ) }
advanceSaga' :: SeasonTime -> Saga -> Saga
advanceSaga' t saga = saga { sagaStates = x:sagaStates saga }
   where x = advanceSagaState saga t (sagaState saga)

-- | Advance the Saga according to timestamp in the SagaFile.
advanceSaga :: SagaFile -> Saga -> Saga
advanceSaga t saga = advanceSaga' (currentSeason t) saga


-- |
-- = Other Markdown Output

-- |
-- == Error reports

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

-- | Character Index
-- ==

-- | Return the canonical file name for the character, without directory.
-- This is currently the full name with a ".md" suffix.
charFileName :: Character -> String
charFileName = (++".md") . fullName 

-- | Write a single item for `characterIndex`
characterIndexLine :: Character -> OList
characterIndexLine c = OString $ "+ " ++ wikiLink (characterStateName c) 

-- | Write a bullet list of links for a list of characters
characterIndex :: [Character] -> OList
characterIndex = OList . map characterIndexLine 

