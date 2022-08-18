{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Parsing and advancement of traits.
--
-- When parsing a trait without an arm:traitClass property, Nothing
-- is returned.  Thus such traits will be discarded.  
--
-- The advancement logic depends on Haskell types rather than
-- working on the RDFGraph.  It is possible that this is slower
-- than necessary.
-- It is important to separate the different advancements into 
-- separata data structures, sort them, and apply them in order.
-- Working with all the advancement in one RDFGraph is expensive.
-- However, the parsing into Haskell types may not be necessary.
--
-----------------------------------------------------------------------------
module ArM.Character.Trait ( Trait(..)
                           , Item(..)
                           , advanceTraitList
                           , advanceItemList
                           ) where

import           Data.Set (fromList)
import           Data.List (sort)
import           Swish.RDF.Graph 
import ArM.Resources
import ArM.KeyPair
import ArM.Rules.Aux
import ArM.Types.Character

data XPType = XP { addXP :: Maybe Int, totalXP :: Maybe Int }
defaultXPType = XP { addXP = Nothing, totalXP = Nothing }

-- |
-- = Trait Advancement

-- | Given one list of Traits and one of Trait advancements,
-- apply each advancement to the corresponding Trait.
-- The lists must be sorted by Trait class name.
advanceTraitList :: [Trait] -> [Trait] -> [Trait]
advanceTraitList xs [] = xs
advanceTraitList [] (y:ys) = recalculateXP y:advanceTraitList [] ys
advanceTraitList (x:xs) (y:ys) 
     | x < y  = x:advanceTraitList xs (y:ys)
     | x > y  = recalculateXP y:advanceTraitList (x:xs) ys
     | otherwise = advanceTraitList ( advanceTrait x y:xs ) ys
     where xc = traitClass x
           yc = traitClass y

-- | apply a given Trait Advancement to a given Trait
-- 1.  apply addedXP
-- 3.  take other properties from the second Trait if available
-- 4.  default to properties from the first Trait
advanceTrait :: Trait -> Trait -> Trait 
advanceTrait trait adv = recalculateXP  $
           trait { traitContents = advanceTriples 
                      ( traitContents trait ) 
                      ( traitContents adv ) 
           }

advanceTriples :: [RDFTriple] -> [RDFTriple] -> [RDFTriple]
advanceTriples x = snd . advanceTriples2 defaultXPType . advanceTriples1 x

advanceTriples1 :: [RDFTriple] -> [RDFTriple] -> [RDFTriple]
advanceTriples1 xs [] = xs
advanceTriples1 [] ys = ys
advanceTriples1 (x:xs) (y:ys) 
    | arcSubj x /=  arcSubj y = error "Conflicting Trait IDs in advanceTriples1."
    | arcPred x < arcPred y = x:advanceTriples (xs) (y:ys)
    | arcPred x > arcPred y = y:advanceTriples (x:xs) (ys)
    | otherwise = x:advanceTriples xs ys

advanceTriples2 :: XPType -> [RDFTriple] -> (XPType,[RDFTriple])
advanceTriples2 xp []  = (xp,[])
advanceTriples2 xp (x:xs) 
   | p == armRes "hasTotalXP" = (xp' { totalXP = val }, xs' )
   | p == armRes "addedXP" = (xp' { addXP = val }, xs' )
   | otherwise = (xp', x:xs' )
           where p = arcPred x
                 (xp',xs') = advanceTriples2 xp xs
                 val = Just $ intFromRDF $ arcObj x

-- |
-- == Recalculation of XP (auxiliary functions

-- | Make a new trait (for a CharacterSheet) from a Trait Advancement.
--  - add addedXP to totalXP (defaulting to 0)
--  - add implied traits
recalculateXP :: Trait -> Trait
recalculateXP x 
     = x { traitContents = makeNewTraitTriples $ traitContents x }

-- | Parse through the Triples of a Trait and recalculate score
-- based on XP
makeNewTraitTriples :: [KeyValuePair] -> [KeyValuePair]
makeNewTraitTriples ts = sort $ x:ys
    where (xp,ys) = makeNewTraitTriples' (defaultXPType,[]) ts
          x = KeyValuePair (totalXPLabel) (toRDFLabel ( totalXP xp + addXP xp ))

-- | Parse through the Triples of a Trait and remove XP related traits
makeNewTraitTriples' :: (XPType,[KeyValuePair]) -> [KeyValuePair] -> (XPType,[KeyValuePair]) 
makeNewTraitTriples' xt [] = xt
makeNewTraitTriples' (xp,ys) (KeyValuePair a c:zs) 
    | a == addXPLabel = makeNewTraitTriples' (xp { addXP = read c },ys) zs
    | a == totalXPLabel = makeNewTraitTriples' (xp { totalXP = read c },ys) zs
    | otherwise       = makeNewTraitTriples' (xp, KeyValuePair a c:ys) zs
    where read = f . fromRDFLabel
          f Nothing = 0
          f (Just x) = x

-- |
-- = Parsing Traits and Items from RDF 

traitKVList :: [KeyValuePair] -> (Maybe RDFLabel,Bool,Bool,Bool,[KeyValuePair])
traitKVList xs = traitKVList' (Nothing,False,False,False,[]) xs
traitKVList' :: (Maybe RDFLabel,Bool,Bool,Bool,[KeyValuePair]) 
                 -> [KeyValuePair]  
                 -> (Maybe RDFLabel,Bool,Bool,Bool,[KeyValuePair])
traitKVList' (k,a,b,c,xs) [] = (k,a,b,c,xs)
traitKVList' (k,a,b,c,xs) (KeyValuePair y2 y4:ys) =
         traitKVList' (f y2 y4 ,a',b',c',KeyValuePair y2 y4:xs) ys
         where  a' = a || y4 == repeatableLabel
                b' = b || y4 == xptraitLabel
                c' = c || y4 == accelleratedtraitLabel
                f x y | x ==  prefixedidRes = Just y
                      | otherwise           = k

