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
module ArM.Cov.Saga ( Saga(..)
                    , SagaFile(..)
                    , SagaState(..)
                    , advanceSaga
                    , sagaStateName
                    , characterIndex
                    ) where

-- import Data.Maybe 
import Data.Aeson 
import Data.List 
import GHC.Generics

-- import ArM.Char.Trait
import ArM.Char.Character
import ArM.Char.Types.Advancement
import ArM.DB.Spell
import ArM.DB.Weapon
import ArM.Cov.Covenant
import ArM.BasicIO
import ArM.Helper

import ArM.Debug.Trace

-- |
-- = Saga type

-- | A Saga as it is processed in memory.
-- Multiple files have to be loaded to generate a Saga object from a `SagaFile`.
data Saga = Saga 
         { sagaTitle :: String
         , sagaStates :: [ SagaState ]
         , rootDir :: String
         , spells :: SpellDB
         , weapons :: WeaponDB
         , armour :: ArmourDB
       }  deriving (Eq)

-- | Get the most recent SagaState
-- This is mainly for backward compatibility
sagaState :: Saga -> SagaState
sagaState = head . sagaStates

instance Show Saga where
   show saga = "Saga: " ++ sagaTitle saga

-- | Saga state at a particular point in time, comprising characters and
-- covenants at that point.
data SagaState = SagaState 
         { stateTitle :: String
         , seasonTime :: SeasonTime
         , covenants :: [Covenant]
         , characters :: [Character]
         }  deriving (Eq,Show)


-- | Get the name of the Saga as recorded in the SagaState
sagaStateName :: SagaState -> String
sagaStateName s = stateTitle s ++ " - " ++ (show $ seasonTime s)

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
-- == Error reports

-- | Exctract a list of validation errors 
advancementErrors :: SagaState -> OList
advancementErrors saga = OList $ map formatOutput vss
    where cs = characters saga
          cvs = map cErrors  cs
          vvs = foldl (++) [] cvs
          vss = sortOn ( \ (_,x,_,_) -> x ) vvs
          formatOutput (cid,_,ssn,vs) = OList 
              [ OString ( cid ++ ": " ++ ssn ),
              OList $ map OString (filterError vs) ]

{-
-- | Get advancement errors for a given character.
-- At Game Start, Char Gen errors are returned.  If the character
-- is already advanced in game, the in-game errors are returned
charErrors :: Character -> OList
charErrors c = renderCharErrors c es
           where es | ps == [] = pregameDesign c
                    | otherwise = ps
                 ps = pastAdvancement c

-- | Format strins for `pregameCharErrors` and `ingameCharErrors`
renderCharErrors :: Character -> [AugmentedAdvancement] -> OList
renderCharErrors c as = OList [ OString $ (cid ++ ": " ++ sstr)
                              . OList $  filterError vs ]
   where (cid,season,sstr,vs) = cErrors c
-}

aaErrors :: Character -> AugmentedAdvancement -> (String,SeasonTime,String,[Validation])
aaErrors c a = (charID c, season a, augHead a, vs )
    where vs = validation  a

cErrors :: Character -> [(String,SeasonTime,String,[Validation])]
cErrors c = map (aaErrors c) as
   where as | ps == [] = pregameDesign c
            | otherwise = ps
         ps = pastAdvancement c

-- | Format a header for `renderCharErrors`
augHead :: AugmentedAdvancement -> String
augHead a = augHead' (season a) (mode a)
-- | Format a header for `renderCharErrors`
augHead' :: SeasonTime -> Maybe String -> String
augHead' NoTime Nothing = ("??" )
augHead' x Nothing = (show x )
augHead' NoTime (Just x) = x
augHead' x (Just z) = (show x  ++ " " ++ z)

-- | Get errors from a list of Validation objects
filterError :: [Validation] -> [String]
filterError (Validated _:xs) = filterError xs
filterError (ValidationError x:xs) = x:filterError xs
filterError [] = []


-- | Character Index
-- ==

-- | Write a single item for `characterIndex`
characterIndexLine :: Character -> OList
characterIndexLine c = OString $ "+ " ++ pagesLink (characterStateName c) 

-- | Write a bullet list of links for a list of characters
characterIndex :: [Character] -> OList
characterIndex = OList . map characterIndexLine 

-- |
-- = Advancement

instance Advance Saga where
   -- advance :: SeasonTime -> a -> a
   advance t saga = saga { sagaStates = x:sagaStates saga }
      where x = advance t (sagaState saga)

   -- step :: a -> a
   step saga = saga { sagaStates = x:sagaStates saga }
      where x = step (sagaState saga)

   -- nextSeason :: a -> SeasonTime
   nextSeason = f . sagaStates
       where f [] = NoTime
             f (x:_) = nextSeason x


-- | Advance the Saga according to timestamp in the SagaFile.
advanceSaga :: SagaFile -> Saga -> Saga
advanceSaga t saga = advanceSaga' (seasons t) saga

advanceSaga' :: [SeasonTime] -> Saga -> Saga
advanceSaga' [] = id
advanceSaga' (x:xs) = trace ("adv> " ++ show x) $ advanceSaga' xs . advance x 


instance Advance SagaState where
   advance t saga 
      | NoTime == ns = trace ("SagaState NoTime "++show t) saga 
      | t < ns = trace ("SagaState t>=ns "++show (t,ns)) saga 
      | otherwise = trace (show (t, ns)) $ advance t $ step saga
     where ns = nextSeason saga

   step saga = saga { stateTitle = stateTitle saga 
                    , seasonTime = ns
                    , covenants = map (advance ns) $ covenants saga
                    , characters = map (advance ns) $ characters saga 
                    }
     where ns = nextSeason saga

   nextSeason saga = foldl min NoTime ss
      where ss = [ nextSeason x | x <- characters saga ]
