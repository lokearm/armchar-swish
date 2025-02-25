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
module ArM.Types.Trait ( Trait(..)
                       , defaultTrait
                       , advanceTraitList
                       ) where

import Data.List (sort)
import Data.Aeson
import ArM.KeyPair
import ArM.Resources
import ArM.Rules.Aux

import Control.Parallel.Strategies (parMap,rpar)

import ArM.Debug.NoTrace

-- | 
-- = Trait

-- | Trait Resource
-- `traitID` and `traitContents` are sufficient to describe the trait.
-- The other fields duplicate information to facilitate searching and
-- sorting.
-- When new traits are created, `traitID` is set to nothing?
-- A blank node is only created when it is written into an RDFGraph.
data Trait = Ability { name :: String, spec :: String, xp :: Int }
           | Characteristic { name :: String, score :: Int, aging :: Int }
           | Art { name :: String, xp :: Int }
           | Spell { name :: String, xp :: Int, mastery :: [String] }
           | PTrait { name :: String, score :: Int }
           | Reputation { name :: String, locale :: String,  xp :: Int }
           | VF { name :: String, cost :: Int }
           | Confidence { score :: Int, points :: Int }
           | Warping { points :: Int }
           | Decrepitude { points :: Int }
           deriving (Show, Ord, Eq)

instance ToJSON Trait where 
    toJSON t = toJSON $ KeyPairList $ toKeyPairList $ traitContents t 
instance FromJSON Trait where 
    parseJSON val = do 
                     v <- parseJSON val
                     return $ f1 v
       where f1 (KeyPairList x ) = defaultTrait { traitContents = map f2 x }
             f2 (KeyValuePair x y) = arc (armRes "unnamedBlankNode") x y

-- |
-- = Trait Advancement

-- | Given one list of Traits and one of Trait advancements,
-- apply each advancement to the corresponding Trait.
-- The lists must be sorted by Trait class name.
advanceTraitList :: [Trait] -> [Trait] -> [Trait]
advanceTraitList xs [] =  xs
advanceTraitList [] ys = parMap rpar fixTrait ys
advanceTraitList (x:xs) (y:ys) 
  | x < y  =  x:advanceTraitList xs (y:ys)
  | x > y  =  fixTrait y:advanceTraitList (x:xs) ys
  | otherwise =  advanceTraitList ( (advanceTrait x y):xs ) ys

-- | Apply a given Trait Advancement to a given Trait
-- 1.  take other properties from the second Trait if available
-- 2.  default to properties from the first Trait
-- The advancement's list of trait statements is assumed not be sorted,
-- and are sorted prior to processing.  The character sheet's OTOH
-- has to be pre-sorted, which is not unfortunately.
advanceTrait :: Trait -> Trait -> Trait 
advanceTrait trait adv = fixTrait
           $ trait { traitContents = map (fixSubj $ traitID trait)
           $ advanceTriples ( traitContents trait ) 
                            ( sort $ traitContents adv ) }
      where fixSubj lab x = arc lab ( arcPred x ) ( arcObj x )

-- | Merge two lists of trait statements.  If a subject/property
-- pair is found in both lists, it is taken only from the former.
-- The lists must be sorted.
advanceTriples :: [RDFTriple] -> [RDFTriple] -> [RDFTriple]
advanceTriples xs [] = xs
advanceTriples [] ys = ys
advanceTriples (x:xs) (y:ys) 
    | arcPred x < arcPred y = x:advanceTriples (xs) (y:ys)
    | arcPred x > arcPred y = y:advanceTriples (x:xs) (ys)
    | otherwise = y:advanceTriples xs ys

-- | When a trait is copied or merged from an advancement,
-- XP need to be recalculated.  This auxiliary is applied
-- by two different functions to do this.
fixTrait :: Trait -> Trait
fixTrait trait =  trace dbg ( trait { traitContents = recalc trait } )
    where recalc = calculateQ trait . calculateXP trait . traitContents 
          dbg = instanceLabel trait ++ (show $ traitXP trait) ++ (show $ traitCountable trait) 

-- |
-- == Recalculation of XP (auxiliary functions

