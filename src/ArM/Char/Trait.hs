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
module ArM.Char.Trait ( Trait(..)
                       ) where

import GHC.Generics
import Data.List (sort)
import Data.Aeson

-- | 
-- = Trait

-- | Trait Resource
-- `traitID` and `traitContents` are sufficient to describe the trait.
-- The other fields duplicate information to facilitate searching and
-- sorting.
-- When new traits are created, `traitID` is set to nothing?
-- A blank node is only created when it is written into an RDFGraph.
data Trait = Ability { name :: String, spec :: Maybe String, xp :: Int }
           | Characteristic { name :: String, score :: Int, aging :: Int }
           | Art { name :: String, xp :: Int }
           | Spell { name :: String, xp :: Int, mastery :: [String] }
           | PTrait { name :: String, score :: Int }
           | Reputation { name :: String, locale :: String,  xp :: Int }
           | VF { name :: String, detail :: Maybe String, cost :: Int }
           | Confidence { score :: Int, points :: Int }
           | Warping { points :: Int }
           | Decrepitude { points :: Int }
           deriving (Show, Ord, Eq, Generic)


instance ToJSON Trait 
instance FromJSON Trait 

-- |
-- = Trait Advancement

