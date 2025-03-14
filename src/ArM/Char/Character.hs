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
import ArM.Char.Internal.Character
import ArM.Char.CharGen

import ArM.Debug.Trace

-- |
-- = Advancement in Game

-- | Apply advancement
applyAdvancement :: Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyAdvancement a cs = (a',cs')
    where a' = prepareAdvancement cs a
          cs' = trace ("old>>"++show old)
              $ trace ("change>>"++show change)
              $ trace ("new>>"++show new)
              $ cs { charTime = season a, traits = new }
          new =  advance change tmp
          tmp =  advance inferred old 
          change = sortTraits $ changes a'
          inferred = sortTraits $ inferredTraits a'
          old = sortTraits $ traits cs


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
advanceCharacter :: SeasonTime -> Character -> Character
advanceCharacter ct c | futureAdvancement c == [] = c
                      | isNothing (state c) = advanceCharacter ct $ prepareCharacter c
                      | ct < ct' = c
                      | otherwise =  advanceCharacter ct $ stepCharacter c 
            where y =  head $ futureAdvancement c
                  ct' =  season y

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
