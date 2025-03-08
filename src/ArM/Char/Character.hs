{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Characters and functions for advancement.
--
-- This module contains types to process characters, including 
-- persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.Char.Character ( Character(..)
                          , defaultCharacter
                          , CharacterConcept(..)
                          , defaultConcept
                          , CharacterState(..)
                          , KeyPairList(..)
                          , KeyPair(..)
                          , FieldValue(..)
                          , prepareCharacter
                          , Advancement(..)
                          , fullName
                          , fullConceptName
                          , advanceCharacter
                          , isGrog
                          ) where

import Data.Maybe 

import ArM.Char.Trait
import ArM.Char.Advancement
import ArM.Char.Validation
import ArM.Char.Internal.Character
import ArM.Char.CharGen
-- import ArM.Debug.Trace

-- |
-- = Advancement



-- |
-- == Char Gen Advancement



-- | Augment and amend the advancements based on current virtues and flaws.
prepareCharGen :: [VF] -> Advancement -> AugmentedAdvancement
prepareCharGen vfs = validate . initialLimits vfs . addInferredTraits 


-- | Compute the initial state if no state is recorded.
prepareCharacter :: Character -> Character
prepareCharacter c 
            | state c /= Nothing = c
            | otherwise = c { state = Just $ cs { charTime = Just "Game start" }
                            , pregameDesign = xs
                            , pregameAdvancement = []
                            }
            where as = pregameAdvancement  c 
                  (xs,cs) = applyCGA vfs as defaultCS
                  vfs = getInitVF c


-- | Apply CharGen advancement
applyCharGenAdv :: [VF] -> Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyCharGenAdv vfs a cs = (a',cs')
    where a' = prepareCharGen vfs a
          cs' = cs { charTime = season a, traits = new }
          new =  advance change tmp
          tmp =  advance inferred old 
          change = sortTraits $ changes a'
          inferred = inferredTraits a'
          old = traits cs

-- | Apply a list of advancements
applyCGA :: [VF] -> [Advancement] -> CharacterState -> ([AugmentedAdvancement],CharacterState)
applyCGA vfs a cs = applyCGA' vfs ([],a,cs)
applyCGA' :: [VF] -> ([AugmentedAdvancement],[Advancement],CharacterState)
                   -> ([AugmentedAdvancement],CharacterState)
applyCGA' _ (xs,[],cs) = (xs,cs)
applyCGA' vfs (xs,y:ys,cs) = applyCGA' vfs (a':xs,ys,cs')
    where (a',cs') = applyCharGenAdv vfs y cs

-- |
-- == In Game Advancement


-- | Apply advancement
applyAdvancement :: Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyAdvancement a cs = (a',cs')
    where a' = prepareAdvancement cs a
          cs' = cs { charTime = season a, traits = new }
          new =  advance change tmp
          tmp =  advance inferred old 
          change = sortTraits $ changes a'
          inferred = inferredTraits a'
          old = traits cs

-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: CharacterState -> Advancement -> AugmentedAdvancement
prepareAdvancement _ = validate . addInferredTraits

{-
-- | Apply a list of advancements
applyAdvancements :: [Advancement] -> CharacterState -> ([AugmentedAdvancement],CharacterState)
applyAdvancements a cs = applyAdvancements' ([],a,cs)
applyAdvancements' :: ([AugmentedAdvancement],[Advancement],CharacterState)
                   -> ([AugmentedAdvancement],CharacterState)
applyAdvancements' (xs,[],cs) = (xs,cs)
applyAdvancements' (xs,y:ys,cs) = applyAdvancements' (a':xs,ys,cs')
    where (a',cs') = applyAdvancement y cs

-}


-- | Advance the character until after the given time.
advanceCharacter :: CharTime -> Character -> Character
advanceCharacter ct c | isNothing (state c) = advanceCharacter ct $ prepareCharacter c
                      | ct > ct' = c
                      | otherwise = stepCharacter c 
            where y = head $ futureAdvancement c
                  ct' = season y

-- | Advance the character one season forward
stepCharacter :: Character -> Character
stepCharacter c = c { state = Just cs 
                            , pastAdvancement = (a:xs)
                            , futureAdvancement = ys 
                            }
            where y = head $ futureAdvancement c
                  ys = tail $ futureAdvancement c
                  xs = pastAdvancement c
                  (a,cs) = applyAdvancement y (fromJust $ state c)
