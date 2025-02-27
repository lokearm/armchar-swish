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
                      , advanceTrait
                       ) where

import GHC.Generics
import Data.List (sort)
import Data.Aeson

-- | 
-- = Trait

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
                             , locale :: Maybe String
                             , mastery :: Maybe [ String ]
                             , score :: Maybe Int
                             , cost :: Maybe Int
                             , points :: Maybe Int 
                             , xp :: Maybe Int 
                             , aging :: Maybe Int
                             }
                             deriving (Ord,Eq,Show,Generic)
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

maybeInt :: Maybe Int -> Int
maybeInt Nothing = 0
maybeInt (Just x) = x
maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y = Just $ maybeInt x + maybeInt y

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
instance FromJSON ProtoTrait 


