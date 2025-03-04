{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Types to Traits and advancement of Traits
--
-- Types to handle Characters and Traits, with some basic associated functions.
--
-- When parsing a trait without an arm:traitClass property, Nothing
-- is returned.  Thus such traits will be discarded.  
--
--
-----------------------------------------------------------------------------
module ArM.Char.Trait ( ProtoTrait(..)
                      , Trait(..)
                      , TraitKey(..)
                      , Ability(..)
                      , Art(..)
                      , Spell(..)
                      , Reputation(..)
                      , VF(..)
                      , advanceTrait
                      , advanceTraits
                      , sortTraits
                      , traitKey
                      , getTrait
                      , computeTrait
                      , (<:)
                      , (>:)
                      , toTrait
                      , advance
                      , filterTrait
                      , defaultPT
                       ) where

import ArM.Debug.Trace
import ArM.GameRules
import GHC.Generics
-- import Data.List (sort)
import Data.Aeson
import Data.Maybe (fromJust,isNothing)
import Data.List (sortBy)

import ArM.Helper

-- | 
-- = ProtoTrait

data ProtoTrait = ProtoTrait { ability :: Maybe String
                             , virtue :: Maybe String
                             , flaw :: Maybe String
                             , characteristic :: Maybe String
                             , art :: Maybe String
                             , spell :: Maybe String
                             , ptrait :: Maybe String
                             , confidence :: Maybe String
                             , reputation :: Maybe String
                             , other :: Maybe String
                             , spec :: Maybe String
                             , detail :: Maybe String
                             , appliesTo :: Maybe TraitKey
                             , locale :: Maybe String
                             , mastery :: Maybe [ String ]
                             , score :: Maybe Int
                             , bonusScore :: Maybe Int
                             , multiplyXP :: Maybe Float
                             , cost :: Maybe Int
                             , points :: Maybe Int 
                             , xp :: Maybe Int 
                             , aging :: Maybe Int
                             }
                             deriving (Ord,Eq,Generic)
defaultPT :: ProtoTrait
defaultPT = ProtoTrait { ability = Nothing
                             , virtue = Nothing
                             , flaw = Nothing
                             , characteristic = Nothing
                             , art = Nothing
                             , spell = Nothing
                             , ptrait = Nothing
                             , confidence = Nothing
                             , reputation = Nothing
                             , other = Nothing
                             , spec = Nothing
                             , detail = Nothing
                             , appliesTo = Nothing
                             , locale = Nothing
                             , mastery = Nothing
                             , score = Nothing
                             , bonusScore = Nothing
                             , multiplyXP = Nothing
                             , cost = Nothing
                             , points = Nothing
                             , xp = Nothing
                             , aging = Nothing
                             }

instance ToJSON ProtoTrait 
instance FromJSON ProtoTrait where
    parseJSON = withObject "ProtoTrait" $ \v -> ProtoTrait
        <$> v .:?  "ability"
        <*> v .:?  "virtue"
        <*> v .:?  "flaw"
        <*> v .:?  "characteristic"
        <*> v .:?  "art"
        <*> v .:?  "spell"
        <*> v .:?  "ptrait"
        <*> v .:?  "confidence"
        <*> v .:?  "reputation"
        <*> v .:?  "other"
        <*> v .:?  "spec"
        <*> v .:?  "detail"
        <*> v .:?  "appliesTo"
        <*> v .:?  "locale"
        <*> v .:?  "mastery"
        <*> v .:?  "score"
        <*> v .:?  "bonusScore"
        <*> v .:?  "multiplyXP"
        <*> v .:?  "cost"
        <*> v .:?  "points"
        <*> v .:?  "xp"
        <*> v .:?  "aging"


-- | 
-- = Trait

data TraitKey = AbilityKey String
           | CharacteristicKey String
           | ArtKey String
           | SpellKey String
           | PTraitKey String
           | ReputationKey String String
           | VFKey String
           | ConfidenceKey 
           | OtherTraitKey String
           deriving (Show, Ord, Eq,Generic )

instance ToJSON TraitKey
instance FromJSON TraitKey
data Ability = Ability { abilityName :: String
                       , speciality :: Maybe String
                       , abilityXP :: Int 
                       , abilityScore :: Int 
                       , abilityBonus :: Int 
                       , abilityMultiplier :: Float
                       , abilityExcessXP :: Int 
                       }
           deriving (Ord, Eq, Generic)
data Characteristic = Characteristic { characteristicName :: String
                                     , charScore :: Int
                                     , agingPoints :: Int }
           deriving (Ord, Eq, Generic)
data Art = Art { artName :: String
               , artXP :: Int 
               , artScore :: Int 
               , artBonus :: Int 
               , artMultiplier :: Float
               , artExcessXP :: Int 
               }
           deriving (Ord, Eq, Generic)
data Spell = Spell { spellName :: String
                   , spellXP :: Int
                   , masteryScore :: Int
                   , spellExcessXP :: Int
                   , spellMultiplier :: Float
                   , masteryOptions :: [String] 
                   }
           deriving (Show, Ord, Eq, Generic)
data PTrait = PTrait { ptraitName :: String, pscore :: Int }
           deriving (Show, Ord, Eq, Generic)
data Reputation = Reputation { reputationName :: String
                             , repLocale :: String
                             ,  repXP :: Int 
                             ,  repScore :: Int 
                             ,  repExcessXP :: Int 
                             }
           deriving (Show, Ord, Eq, Generic)
data VF = VF { vfname :: String
             , vfDetail :: String
             , vfcost :: Int 
             , vfAppliesTo :: Maybe TraitKey
             }
           deriving (Show, Ord, Eq, Generic)
data Confidence = Confidence { cscore :: Int, cpoints :: Int }
           deriving (Show, Ord, Eq, Generic)
data OtherTrait = OtherTrait { trait :: String
                             , otherScore :: Int
                             , pts :: Int 
                             , otherExcess :: Int
                             }
           deriving (Show, Ord, Eq, Generic)
data Trait = AbilityTrait Ability
           | CharacteristicTrait Characteristic
           | ArtTrait Art
           | SpellTrait Spell
           | PTraitTrait PTrait
           | ReputationTrait Reputation
           | VFTrait VF
           | ConfidenceTrait Confidence
           | OtherTraitTrait OtherTrait
           deriving (Show, Ord, Eq, Generic)
instance FromJSON Ability
instance FromJSON Characteristic 
instance FromJSON Art 
instance FromJSON Spell 
instance FromJSON PTrait 
instance FromJSON Reputation 
instance FromJSON VF 
instance FromJSON Confidence 
instance FromJSON OtherTrait 
instance FromJSON Trait  
instance ToJSON Ability
instance ToJSON Characteristic 
instance ToJSON Art 
instance ToJSON Spell 
instance ToJSON PTrait 
instance ToJSON Reputation 
instance ToJSON VF 
instance ToJSON Confidence 
instance ToJSON OtherTrait 
instance ToJSON Trait 

showBonus :: Int -> String
showBonus x | x > 0 = " +" ++ show x
            | x < 0 = " " ++ show x
            | otherwise = ""
showSigned :: Int -> String
showSigned x | x > 0 = "+" ++ show x
            | otherwise = show x


instance Show Ability  where
   show a = abilityName a ++ " [" ++ showspec sp ++ "] "
          ++ show (abilityScore a) 
          ++ showBonus (abilityBonus a)
          ++ " (" ++ show (abilityExcessXP a) ++ "xp) "
      where showspec Nothing = "  --  "
            showspec (Just s) = s
            sp = speciality a
instance Show Characteristic  where
   show a = characteristicName a ++ " " ++ showSigned (charScore a)
          ++ showA (agingPoints a)
       where showA x | x == 0 = ""
                    | otherwise = " (" ++ show x ++ " aging points)"
instance Show Art  where
   show a = artName a ++ " " 
          ++ show (artScore a) 
          ++ showBonus (artBonus a)
          ++ " (" ++ show (artExcessXP a) ++ "xp) "

showAging :: ProtoTrait -> String
showAging p | Nothing == aging p = ""
            | otherwise = " (" ++ show  pt ++ " aging points)"
    where pt = maybeInt $ aging p
showXP :: ProtoTrait -> String
showXP p = " " ++ show ( maybeInt (xp p) ) ++ "xp"

instance Show ProtoTrait  where
   show p 
       | ability p /= Nothing = 
           "Ability: " ++ fromJust ( ability p )  ++ 
           " [" ++ show ( spec p ) ++ "]" ++ showXP p
           ++ " [" ++ si (bonusScore p) ++ "; " ++ maybeShow (multiplyXP p) ++ "]"
       | characteristic p /= Nothing =
           "Characteristic: " ++ fromJust ( characteristic p )  ++
           " " ++ show ( maybeInt (score p) ) ++ showAging p 
       | art p /= Nothing = 
           "Art: " ++ fromJust ( art p ) ++ showXP p
           ++ " [" ++ si (bonusScore p) ++ "; " ++ maybeShow (multiplyXP p) ++ "]"
       | spell p /= Nothing =
              "Spell: " ++ fromJust (spell p) ++ showXP p
                    ++ maybeShow (mastery p)
       | ptrait p /= Nothing = 
              "Personality Trait: " ++ fromJust (ptrait p)
                     ++ " " ++ maybeShow (score p)
       | reputation p /= Nothing = 
              "Reputation: " ++ fromJust (reputation p) ++
              " [" ++ maybeShow (locale p) ++ "]" ++ showXP p
       | virtue p /= Nothing = 
              "Virtue: " ++ fromJust (virtue p) ++ " ("
              ++ show ( maybeInt (cost p) ) ++ ")"
       | flaw p /= Nothing = 
              "Flaw: " ++ fromJust (flaw p) ++ " ("
              ++ show ( maybeInt (cost p) ) ++ ")"
       | confidence p /= Nothing = 
              "Confidence: " ++ show (maybeInt (score p)) ++ " (" ++
              show ( maybeInt (points p) ) ++ ")"
       | other p /= Nothing = 
               fromJust (other p) ++ " " ++ show ( maybeInt ( points p ) )
       | otherwise  = error "No Trait for this ProtoTrait" 
     where si = show . maybeInt


-- |
-- = Computing Traits

getAbilityScore :: Maybe Int -> (Int,Int)
getAbilityScore x' = (s,y) 
     where y = x - 5*xpFromScore s
           s = scoreFromXP (x `div` 5)
           x = maybeInt x'

computeOther :: ProtoTrait -> OtherTrait
computeOther p
    | spell p == Nothing = error "Not an properly formatted trait"
    | otherwise =
           OtherTrait { trait = fromJust (other p) 
                      , pts = maybeInt ( points p ) 
                      , otherScore = s
                      , otherExcess = y
                      }
                 where (s,y) = getAbilityScore (points p)



-- |
-- = Filtering and Advancement - the TraitType class

class TraitType t where
    -- | Extract traits of the given type from a generic list of Trait objects.
    -- It returns a pair of lists with the selected traits in the first list
    -- and the remaining traits in the other.
    filterTrait :: [ Trait ] -> ( [ t ], [ Trait ] )
    filterTrait ts = y where (_,y) = filterTrait' (ts,([],[]))

    -- | Recursive helper for `filterTrait`
    filterTrait' :: ( [ Trait ], ( [ t ], [ Trait ] ) )
                  -> ( [ Trait ], ( [ t ], [ Trait ] ) )
    filterTrait' ([],y) = ([],y)
    filterTrait' (x:xs,(ys,zs)) | isNothing ab  = (xs,(ys,x:zs))
                                | otherwise = (xs,(fromJust ab:ys,zs))
        where ab = getTrait x

    -- | Return the specific trait from the generic Trait, or Nothing if the type does not match.
    getTrait :: Trait -> Maybe t

    -- | Convert a ProtoTrait (advancement) to a new trait object.
    computeTrait :: ProtoTrait -> Maybe t

instance TraitType Characteristic where
    getTrait (CharacteristicTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p
       | characteristic p == Nothing = Nothing
       | otherwise = Just $
          Characteristic { characteristicName = fromJust ( characteristic p ) 
                , charScore = maybeInt (score p)
                , agingPoints = maybeInt (aging p) }
instance TraitType VF where
    getTrait (VFTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p 
       | virtue p /= Nothing = Just $ vf1 { vfname = fromJust (virtue p) }
       | flaw p /= Nothing = Just $ vf1 { vfname = fromJust (flaw p) }
       | otherwise = Nothing
      where vf1 = VF { vfname = "", vfcost = maybeInt (cost p), vfDetail = maybeString $ detail p,
                    vfAppliesTo = Nothing }
instance TraitType Ability where
    getTrait (AbilityTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p
       | ability p == Nothing = Nothing
       | otherwise = Just $
           Ability { abilityName = fromJust ( ability p ) 
                , speciality = spec p
                , abilityXP = maybeInt (xp p)
                , abilityScore = s
                , abilityExcessXP = y
                , abilityBonus = maybeInt $ bonusScore p
                , abilityMultiplier = maybeInt $ multiplyXP p
                }
     where (s,y) = getAbilityScore (xp p)
instance TraitType Art where
    getTrait (ArtTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p
        | art p == Nothing = Nothing
        | otherwise = Just $
            Art { artName = fromJust ( ability p ) 
                , artXP = x
                , artScore = s
                , artExcessXP = y
                , artBonus = maybeInt $ bonusScore p
                , artMultiplier = maybeInt $ multiplyXP p
                }
     where y = x - xpFromScore s
           s = scoreFromXP x
           x = maybeInt (xp p) 
instance TraitType Spell where
    getTrait (SpellTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p
        | spell p == Nothing = Nothing
        | otherwise = Just $
           Spell { spellName = fromJust (spell p)
                      , spellXP = maybeInt (xp p)
                      , masteryScore = s
                      , masteryOptions = maybeList (mastery p)
                      , spellExcessXP = y
                      , spellMultiplier = maybeInt $ multiplyXP p
                      }
     where (s,y) = getAbilityScore (xp p)
instance TraitType Reputation where
    getTrait (ReputationTrait x) = Just x
    getTrait _ = Nothing
    computeTrait p
       | reputation p == Nothing = Nothing
       | otherwise = Just $
           Reputation { reputationName = fromJust (reputation p)
                      , repLocale = maybeString (locale p)
                      , repXP = maybeInt (xp p)
                      , repScore = s
                      , repExcessXP = y
                      }
     where (s,y) = getAbilityScore (xp p)


-- |
-- = Sorting and Advancement 

(<:) :: (TraitLike t1, TraitLike t2) => t1 -> t2 -> Bool
(<:) p1 p2 = traitKey p1 < traitKey p2

(>:) :: (TraitLike t1, TraitLike t2) => t1 -> t2 -> Bool
(>:) p1 p2 = p2 <: p1


sortTraits :: TraitLike t => [ t ] -> [ t ]
sortTraits = sortBy f
       where f x y | x <: y = LT
                   | y <: x = GT
                   | otherwise = EQ

-- |
-- == the TraitLike class

class TraitLike t where
    traitKey :: t -> TraitKey
    advanceTrait :: ProtoTrait -> t -> t
    advanceTrait _ x = x
    toTrait :: t -> Trait

instance TraitLike Trait where
    traitKey (CharacteristicTrait x) = traitKey x
    traitKey (AbilityTrait x) = traitKey x
    traitKey (ArtTrait x) = traitKey x
    traitKey (SpellTrait x) = traitKey x
    traitKey (ReputationTrait x) = traitKey x
    traitKey (VFTrait x) = traitKey x
    traitKey (PTraitTrait x) = traitKey x
    traitKey (OtherTraitTrait x) = traitKey x
    traitKey (ConfidenceTrait x) = traitKey x
    advanceTrait a (CharacteristicTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (AbilityTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (ArtTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (SpellTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (ReputationTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (VFTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (PTraitTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (OtherTraitTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (ConfidenceTrait x) = toTrait $ advanceTrait a x
    toTrait = id

instance TraitLike PTrait where
    traitKey x = PTraitKey $ ptraitName x
    toTrait = PTraitTrait
    advanceTrait _ x = trace "Warning! Advancement not implemented for personality traits"  x
instance TraitLike VF where
    traitKey x = VFKey $ vfname x
    toTrait = VFTrait
instance TraitLike Ability where
    traitKey x = AbilityKey $ abilityName x
    toTrait = AbilityTrait
    advanceTrait a x = 
      trace (show x) $ trace (show a) $ updateAbilitySpec (spec a) $ updateAbilityXP y x
      where y = (abilityExcessXP x) + (maybeInt $ xp a)
instance TraitLike Art where
    traitKey x = ArtKey $ artName x
    toTrait = ArtTrait
    advanceTrait a x = updateArtXP y x
      where y = (artExcessXP x) + (maybeInt $ xp a)
instance TraitLike Spell where
    traitKey x = SpellKey $ spellName x
    toTrait = SpellTrait
    advanceTrait a x = trace (show x) $ trace (show a) $ 
        updateSpellXP y $ updateSpellMastery ms x
      where y = (spellExcessXP x) + (maybeInt $ xp a)
            ms = maybeList $ mastery a
instance TraitLike Reputation where
    traitKey x = ReputationKey ( reputationName x ) ( repLocale x )
    toTrait = ReputationTrait
    advanceTrait a x = updateRepXP y x
      where y = (repExcessXP x) + (maybeInt $ xp a)
instance TraitLike Characteristic where
    traitKey x = CharacteristicKey ( characteristicName x ) 
    toTrait = CharacteristicTrait
    advanceTrait _ x = trace "Warning! Advancement not implemented for characteristics"  x
instance TraitLike Confidence where
    traitKey _ = ConfidenceKey 
    toTrait = ConfidenceTrait
    advanceTrait a = updateCScore (score a) . updateCPoints (points a) 
       where updateCScore Nothing x = x
             updateCScore (Just y) x = x { cscore = y }
             updateCPoints Nothing x = x
             updateCPoints (Just y) x = x { cpoints = y + cpoints x }
instance TraitLike  OtherTrait where
    traitKey x = OtherTraitKey ( trait x ) 
    toTrait = OtherTraitTrait
    advanceTrait _ x = trace "Warning! Advancement not implemented for OtherTrait"  x


instance TraitLike ProtoTrait where
   traitKey p
       | ability p /= Nothing = AbilityKey $ fromJust $ ability p 
       | characteristic p /= Nothing = CharacteristicKey $ fromJust $ characteristic p 
       | art p /= Nothing = ArtKey $ fromJust $ art p 
       | spell p /= Nothing = SpellKey $ fromJust $ spell p
       | ptrait p /= Nothing = PTraitKey $ fromJust $ ptrait p
       | reputation p /= Nothing = ReputationKey (fromJust (reputation p)) (maybeString (locale p))
       | virtue p /= Nothing = VFKey $ fromJust (virtue p)
       | flaw p /= Nothing = VFKey $ fromJust (flaw p)
       | confidence p /= Nothing = ConfidenceKey 
       | other p /= Nothing = OtherTraitKey $ fromJust $ other p
       | otherwise  = error "No Trait for this ProtoTrait" 

   advanceTrait a
       | ability a /= Nothing = updateSpec a . updateXP a
       | characteristic a /= Nothing = updateScore a . updateAging a
       | art a /= Nothing = updateXP a 
       | spell a /= Nothing = updateXP a . updateMastery a
       | ptrait a /= Nothing = updateScore a 
       | reputation a /= Nothing = updateScore a 
       | confidence a /= Nothing = updateScore a . updatePts a
       | other a /= Nothing = updatePts a
       | otherwise  = id
   toTrait p 
      | ability p /= Nothing = AbilityTrait $ fromJust $ computeTrait p
      | characteristic p /= Nothing = CharacteristicTrait $  fromJust $ computeTrait p
      | art p /= Nothing = ArtTrait $ fromJust $ computeTrait p
      | spell p /= Nothing = SpellTrait $ fromJust $ computeTrait p
      | ptrait p /= Nothing = PTraitTrait $
           PTrait { ptraitName = fromJust (ptrait p)
                  , pscore = maybeInt (score p)
                  } 
      | reputation p /= Nothing = ReputationTrait $ fromJust $ computeTrait p
      | virtue p /= Nothing = VFTrait $ fromJust $ computeTrait p
      | flaw p /= Nothing = VFTrait $ fromJust $ computeTrait p
      | confidence p /= Nothing = ConfidenceTrait $
           Confidence { cscore = maybeInt (score p), cpoints = maybeInt (points p) }
      | other p /= Nothing = OtherTraitTrait $ computeOther p
      | otherwise  = error "No Trait for this ProtoTrait" 

-- |
-- == Auxiliary update functions

updateAbilitySpec :: Maybe String -> Ability -> Ability
updateAbilitySpec Nothing a = a
updateAbilitySpec (Just x) a = a { speciality = Just x }
updateAbilityXP :: Int -> Ability -> Ability
updateAbilityXP x ab | x < tr = ab { abilityExcessXP = x }
                     | otherwise = updateAbilityXP (x-tr) $ ab { abilityScore = sc+1 }
    where sc = abilityScore ab
          tr = (sc+1)*5
updateRepXP :: Int -> Reputation -> Reputation
updateRepXP x ab | x < tr = ab { repExcessXP = x }
                     | otherwise = updateRepXP (x-tr) $ ab { repScore = sc+1 }
    where sc = repScore ab
          tr = (sc+1)*5

updateArtXP :: Int -> Art -> Art
updateArtXP x ab | x < tr = ab { artExcessXP = x }
                     | otherwise = updateArtXP (x-tr) $ ab { artScore = sc+1 }
    where sc = artScore ab
          tr = (sc+1)

updateSpellXP :: Int -> Spell -> Spell
updateSpellXP x ab | x < tr = ab { spellExcessXP = x }
                   | otherwise = updateSpellXP (x-tr) $ ab { masteryScore = sc+1 }
    where sc = masteryScore ab
          tr = (sc+1)*5
updateSpellMastery :: [String] -> Spell -> Spell
updateSpellMastery ms t = t { masteryOptions = (masteryOptions t) ++ ms }

updateSpec :: ProtoTrait -> ProtoTrait -> ProtoTrait
updateSpec a = u (spec a)
    where u Nothing t = t
          u (Just x) t = t { spec = Just x }
updateScore :: ProtoTrait -> ProtoTrait -> ProtoTrait
updateScore a = u (score a)
    where u Nothing t = t
          u (Just x) t = t { score = Just x }
updateXP :: ProtoTrait -> ProtoTrait -> ProtoTrait
updateXP a t = t { xp = maybeAdd (xp a) (xp t) }
updatePts :: ProtoTrait -> ProtoTrait -> ProtoTrait
updatePts a t = t { points = maybeAdd ( points t ) ( points a ) }
updateAging :: ProtoTrait -> ProtoTrait -> ProtoTrait
updateAging a t = t { aging = maybeAdd ( aging t ) ( aging a ) }
updateMastery :: ProtoTrait -> ProtoTrait -> ProtoTrait
updateMastery a t = t { mastery = f (mastery t) (mastery a) }
    where f Nothing x = x
          f y Nothing = y
          f (Just x) (Just y)  = Just (x ++ y)


-- | Merge two lists of ProtoTrait (advancement) objects, combining objects
-- which refer to the same Trait.
advanceTraits :: [ ProtoTrait ] -> [ ProtoTrait ] -> [ ProtoTrait ]
advanceTraits [] ys = ys
advanceTraits ys [] = ys
advanceTraits (x:xs) (y:ys) 
    | x <: y = x:advanceTraits xs (y:ys)
    | y <: x = y:advanceTraits (x:xs) ys
    | otherwise = advanceTrait x y:advanceTraits xs ys

-- | Apply a list of ProtoTrait advancements to a list of Traits.
advance :: [ ProtoTrait ] -> [ Trait ] -> [ Trait ]
advance [] ys = ys
advance ys [] = map toTrait ys
advance (x:xs) (y:ys) 
    | x <: y = toTrait x:advance xs (y:ys)
    | y <: x = y:advance (x:xs) ys
    | otherwise = adv x y:advance xs ys
    where adv a b = toTrait $ advanceTrait a b

