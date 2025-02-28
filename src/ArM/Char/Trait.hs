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
                      , processTrait
                      , key
                      , (<:)
                       ) where

import GHC.Generics
-- import Data.List (sort)
import Data.Aeson
import Data.Maybe (fromJust)

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
                             deriving (Ord,Eq,Show,Generic)

key :: ProtoTrait -> ( Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String
                             , Maybe String, Maybe String, Maybe String, Maybe String
                             , Maybe String, Maybe String, Maybe Int )
key pt = ( ability pt, virtue pt, flaw pt, characteristic pt, art pt, spell pt
                             , ptrait pt , confidence pt , reputation pt , other pt
                             , detail pt , locale pt , cost pt )

(<:) :: ProtoTrait -> ProtoTrait -> Bool
(<:) p1 p2 = key p1 < key p2


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

data Trait = Ability { abilityName :: String, speciality :: Maybe String, abilityXP :: Int }
           | Characteristic { characteristicName :: String, charScore :: Int, agingPoints :: Int }
           | Art { artName :: String, artXP :: Int }
           | Spell { spellName :: String, spellXP :: Int, masteryOptions :: [String] }
           | PTrait { ptraitName :: String, pscore :: Int }
           | Reputation { reputationName :: String, repLocale :: String,  repXP :: Int }
           | VF { vfname :: String, vfcost :: Int }
           | Confidence { cscore :: Int, cpoints :: Int }
           | OtherTrait { trait :: String, pts :: Int }
           deriving (Show, Ord, Eq)

processTrait :: ProtoTrait -> Trait
processTrait p
    | ability p /= Nothing = 
        Ability { abilityName = fromJust ( ability p ) 
                , speciality = spec p
                , abilityXP = maybeInt (xp p) }
    | characteristic p /= Nothing =
        Characteristic { characteristicName = fromJust ( characteristic p ) 
                , charScore = maybeInt (score p)
                , agingPoints = maybeInt (aging p) }
    | art p /= Nothing = 
        Art { artName = fromJust ( art p ) 
            , artXP = maybeInt (xp p) }
    | spell p /= Nothing =
           Spell { spellName = fromJust (spell p)
                 , spellXP = maybeInt (xp p) 
                 , masteryOptions = maybeList (mastery p)
                 }
    | ptrait p /= Nothing = 
           PTrait { ptraitName = fromJust (ptrait p)
                  , pscore = maybeInt (score p)
                  } 
    | reputation p /= Nothing = 
           Reputation { reputationName = fromJust (reputation p)
                      , repLocale = maybeString (locale p)
                      , repXP = maybeInt (xp p)
                      }
    | virtue p /= Nothing = 
           VF { vfname = fromJust (virtue p), vfcost = maybeInt (cost p) }
    | flaw p /= Nothing = 
           VF { vfname = fromJust (flaw p), vfcost = maybeInt (cost p) }
    | confidence p /= Nothing = 
           Confidence { cscore = maybeInt (score p), cpoints = maybeInt (points p) }
    | other p /= Nothing = 
           OtherTrait { trait = fromJust (other p) 
                      , pts = maybeInt ( points p ) }
    | otherwise  = error "No Trait for this ProtoTrait" 
