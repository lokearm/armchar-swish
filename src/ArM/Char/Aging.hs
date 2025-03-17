{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Aging
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Types to manage the aging state and progression
--
--
-----------------------------------------------------------------------------
module ArM.Char.Aging where

-- import ArM.GameRules
-- import ArM.Helper
import ArM.Char.Trait

import GHC.Generics
import Data.Aeson
import Data.Maybe 
-- import Data.List (sortBy)

-- import ArM.Debug.Trace

data Age = Age
    { ageYears :: Int             -- ^ character age in years
    , apparentYounger :: Int      -- ^ difference between age and apparent age
    , ageComment :: Maybe String  -- ^ freeform comment
    } deriving (Show,Ord,Eq,Generic)
instance ToJSON Age
instance FromJSON Age 

data Aging = Aging
    { apparentChange :: Int       -- ^ difference between age and apparent age
    , agingeRollDie  :: Int       -- ^ aging roll die result
    , agingeRoll     :: Int       -- ^ aging roll total
    , agingComment   :: Maybe String  -- ^ freeform comment
    } deriving (Show,Ord,Eq,Generic)
instance ToJSON Aging
instance FromJSON Aging 
