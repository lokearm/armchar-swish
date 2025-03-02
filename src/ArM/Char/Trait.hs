{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
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
                      , advanceTrait
                      , advanceTraits
                      , processTrait
                      , sortTraits
                      , key
                      , getTraitKey
                      , (<:)
                       ) where

import ArM.GameRules
import GHC.Generics
-- import Data.List (sort)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.List (sortBy)

maybeList :: Maybe [a] -> [a]
maybeList Nothing = []
maybeList (Just x) = x
maybeString :: Maybe String -> String
maybeString Nothing = ""
maybeString (Just x) = x
maybeInt :: Maybe Int -> Int
maybeInt Nothing = 0
maybeInt (Just x) = x
maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y = Just $ maybeInt x + maybeInt y

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
                             , locale :: Maybe String
                             , mastery :: Maybe [ String ]
                             , score :: Maybe Int
                             , cost :: Maybe Int
                             , points :: Maybe Int 
                             , xp :: Maybe Int 
                             , aging :: Maybe Int
                             }
                             deriving (Ord,Eq,Generic)

key :: ProtoTrait -> ( Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String
                             , Maybe String, Maybe String, Maybe String, Maybe String
                             , Maybe String, Maybe String, Maybe Int )
key pt = ( ability pt, virtue pt, flaw pt, characteristic pt, art pt, spell pt
                             , ptrait pt , confidence pt , reputation pt , other pt
                             , detail pt , locale pt , cost pt )
class GetTraitKey a where
   getTraitKey :: a -> TraitKey
instance GetTraitKey ProtoTrait where
   getTraitKey p
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
instance GetTraitKey Trait where
   getTraitKey (AbilityTrait a) = AbilityKey $ abilityName a
   getTraitKey (ArtTrait a) = ArtKey $ artName a
   getTraitKey (SpellTrait a) = SpellKey $ spellName a
   getTraitKey (CharacteristicTrait a) = CharacteristicKey $ characteristicName a
   getTraitKey (PTraitTrait a) = PTraitKey $ ptraitName a
   getTraitKey (ReputationTrait a) = ReputationKey ( reputationName a ) (repLocale a)
   getTraitKey (ConfidenceTrait _) = ConfidenceKey
   getTraitKey (VFTrait a) = VFKey $ vfname a
   getTraitKey (OtherTraitTrait a) = OtherTraitKey $ trait a

class TraitCompare a where
    (<:) :: a -> ProtoTrait -> Bool
instance TraitCompare ProtoTrait where
    (<:) p1 p2 = getTraitKey p1 < getTraitKey p2
instance TraitCompare Trait where
    (<:) p1 p2 = getTraitKey p1 < getTraitKey p2

sortTraits :: [ ProtoTrait ] -> [ ProtoTrait ]
sortTraits = sortBy f
      where f x y | x <: y = LT
                  | y <: x = GT
                  | otherwise = EQ


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

advanceTrait :: ProtoTrait -> ProtoTrait -> ProtoTrait
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

advanceTraits :: [ ProtoTrait ] -> [ ProtoTrait ] -> [ ProtoTrait ]
advanceTraits [] ys = ys
advanceTraits ys [] = ys
advanceTraits (x:xs) (y:ys) 
    | x <: y = x:advanceTraits xs (y:ys)
    | y <: x = y:advanceTraits (x:xs) ys
    | otherwise = advanceTrait x y:advanceTraits xs ys


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
        <*> v .:?  "locale"
        <*> v .:?  "mastery"
        <*> v .:?  "score"
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
           deriving (Show, Ord, Eq )
data Ability = Ability { abilityName :: String
                       , speciality :: Maybe String
                       , abilityXP :: Int 
                       , abilityScore :: Int 
                       , abilityBonus :: Int 
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
               , artExcessXP :: Int 
               }
           deriving (Ord, Eq, Generic)
data Spell = Spell { spellName :: String
                   , spellXP :: Int
                   , masteryScore :: Int
                   , spellExcessXP :: Int
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
data VF = VF { vfname :: String, vfDetail :: String, vfcost :: Int }
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
maybeShow :: Show a => Maybe a -> String
maybeShow Nothing = ""
maybeShow (Just x) = show x

instance Show ProtoTrait  where
   show p 
    | ability p /= Nothing = 
        "Ability: " ++ fromJust ( ability p )  ++ 
        " [" ++ show ( spec p ) ++ "]" ++ showXP p
    | characteristic p /= Nothing =
        "Characteristic: " ++ fromJust ( characteristic p )  ++
        " " ++ show ( maybeInt (score p) ) ++ showAging p 
    | art p /= Nothing = 
        "Art: " ++ fromJust ( art p ) ++ showXP p
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


-- |
-- = Computing Traits
computeAbility :: ProtoTrait -> Ability
computeAbility p
    | ability p == Nothing = error "Not an ability"
    | otherwise =
        Ability { abilityName = fromJust ( ability p ) 
                , speciality = spec p
                , abilityXP = maybeInt (xp p)
                , abilityScore = s
                , abilityExcessXP = y
                , abilityBonus = 0
                }
     where (s,y) = getAbilityScore (xp p)
computeArt :: ProtoTrait -> Art
computeArt p
    | art p == Nothing = error "Not an art"
    | otherwise =
        Art { artName = fromJust ( ability p ) 
                , artXP = x
                , artScore = s
                , artExcessXP = y
                , artBonus = 0 
                }
     where y = x - xpFromScore s
           s = scoreFromXP x
           x = maybeInt (xp p) 

getAbilityScore :: Maybe Int -> (Int,Int)
getAbilityScore x' = (s,y) 
     where y = x - 5*xpFromScore s
           s = scoreFromXP (x `div` 5)
           x = maybeInt x'

computeReputation :: ProtoTrait -> Reputation
computeReputation p
    | reputation p == Nothing = error "Not an reputation"
    | otherwise =
           Reputation { reputationName = fromJust (reputation p)
                      , repLocale = maybeString (locale p)
                      , repXP = maybeInt (xp p)
                      , repScore = s
                      , repExcessXP = y
                      }
     where (s,y) = getAbilityScore (xp p)
computeSpell :: ProtoTrait -> Spell
computeSpell p
    | spell p == Nothing = error "Not an spell"
    | otherwise =
           Spell { spellName = fromJust (spell p)
                      , spellXP = maybeInt (xp p)
                      , masteryScore = s
                      , masteryOptions = maybeList (mastery p)
                      , spellExcessXP = y
                      }
     where (s,y) = getAbilityScore (xp p)
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

processTrait :: ProtoTrait -> Trait
processTrait p
    | ability p /= Nothing = AbilityTrait $ computeAbility p
    | characteristic p /= Nothing = CharacteristicTrait $
        Characteristic { characteristicName = fromJust ( characteristic p ) 
                , charScore = maybeInt (score p)
                , agingPoints = maybeInt (aging p) }
    | art p /= Nothing = ArtTrait $ computeArt p
    | spell p /= Nothing = SpellTrait $ computeSpell p
    | ptrait p /= Nothing = PTraitTrait $
           PTrait { ptraitName = fromJust (ptrait p)
                  , pscore = maybeInt (score p)
                  } 
    | reputation p /= Nothing = ReputationTrait $ computeReputation p
    | virtue p /= Nothing = VFTrait $
           VF { vfname = fromJust (virtue p), vfcost = maybeInt (cost p), vfDetail = maybeString $ detail p }
    | flaw p /= Nothing = VFTrait $
           VF { vfname = fromJust (flaw p), vfcost = maybeInt (cost p), vfDetail = maybeString $ detail p }
    | confidence p /= Nothing = ConfidenceTrait $
           Confidence { cscore = maybeInt (score p), cpoints = maybeInt (points p) }
    | other p /= Nothing = OtherTraitTrait $ computeOther p
    | otherwise  = error "No Trait for this ProtoTrait" 

