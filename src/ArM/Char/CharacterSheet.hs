-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.CharacterSheet
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Char.CharacterSheet where

data CharacterSheet = CharacterSheet 
         { charTime :: CharTime
         , vfList :: [ VF ]
         , abilityList :: [ Ability ]
         , artList :: [ Art ]
         , spellList :: [ Spell ]
         , reputationList :: [ Reputation ]
         , traits :: [ Trait ]
         }  deriving (Eq,Generic)

defaultheet :: Characterheet 
defaultheet = Characterheet 
         { charTime = Nothing
         , vfList = [ ]
         , abilityList = [ ]
         , artList = [ ]
         , spellList = [ ]
         , reputationList = []
         , traits = [ ]
         }  

instance ToJSON CharacterSheet where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CharacterSheet where
    parseJSON = withObject "CharacterSheet" $ \v -> CharacterSheet
        <$> v .:? "charTime"
        <*> fmap listNothing ( v .:? "vfList" )
        <*> fmap listNothing ( v .:? "abilityList" )
        <*> fmap listNothing ( v .:? "artList" )
        <*> fmap listNothing ( v .:? "spellList" )
        <*> fmap listNothing ( v .:? "reputationList" )
        <*> fmap listNothing ( v .:? "traits" )

filterCS :: CharacterState -> CharacterSheet
filterCS cs = cs { vfList = x1
                 , abilityList = x2
                 , artList = x3
                 , spellList = x4
                 , reputationList = x5
                 , traits = y5
                }
           where (x1,y1) = filterTrait $ traits cs
                 (x2,y2) = filterTrait y1
                 (x3,y3) = filterTrait y2
                 (x4,y4) = filterTrait y3
                 (x5,y5) = filterTrait y4
