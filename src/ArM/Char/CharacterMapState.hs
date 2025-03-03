module ArM.Char.CharacterMapState where

import qualified Data.Map as Map

data CharacterMapState = CharacterMapState 
         { cmsTime :: CharTime
         , traitMap :: Map.Map TraitKey Trait
         }  deriving (Eq,Generic)
defaultCMS :: CharacterMapState 
defaultCMS = CharacterMapState 
         { cmsTime = Nothing
         , traitMap = Map.empty
         }

