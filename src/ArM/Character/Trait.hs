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
                           , advanceTraitList
                           ) where

import           Data.Set (fromList)
import           Data.List (sort)
import           Swish.RDF.Graph 
import ArM.Resources
import ArM.Rules.Aux
import ArM.Types.Character
import ArM.Types.Trait

import Control.Parallel.Strategies (parMap,rpar)

-- import Debug.Trace
trace x y = y


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
     where xc = traitClass x
           yc = traitClass y

-- | Apply a given Trait Advancement to a given Trait
-- 1.  take other properties from the second Trait if available
-- 2.  default to properties from the first Trait
-- The advancement's list of trait statements is assumed not be sorted,
-- and are sorted prior to processing.  The character sheet's OTOH
-- has to be pre-sorted, which is not unfortunately.
advanceTrait :: Trait -> Trait -> Trait 
advanceTrait trait adv = 
  fixTrait $ trait { traitContents = map fixSubj 
           $ advanceTriples ( traitContents trait ) 
                            ( sort $ traitContents adv ) }
      where fixSubj x = arc ( armRes "unnamedBlankNode" ) 
                            ( arcPred x ) ( arcObj x )

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
fixTrait trait = trace "fixTrait" $ trait {
       traitContents = sort $ calculateXP $ traitContents trait
    }

-- |
-- == Recalculation of XP (auxiliary functions

-- | Auxiliary for `fixTrait`
calculateXP :: [RDFTriple] -> [RDFTriple]
calculateXP ts = trace ("calculateXP\n"++show ts) $ makeXParc xs ys 
   where (xs,ys) = getXPtriples ts
         makeXParc [] ys = ys
         makeXParc xs ys = getXParc xs:ys

-- | Auxiliary for `calculateXP`
getXParc (x:xs) = trace ("getXParc "++show xp) $
                  arc (arcSubj x) (armRes "hasTotalXP") (litInt xp)
   where xp = foldr (+) 0 $ map ( intFromRDF . arcObj ) (x:xs)

-- | Auxiliary for `calculateXP`
getXPtriples :: [RDFTriple] -> ([RDFTriple],[RDFTriple])
getXPtriples xs = trace "getXPtriples" $ getXPtriples' ([],xs)

-- | Auxiliary for `calculateXP`
getXPtriples' :: ([RDFTriple],[RDFTriple]) -> ([RDFTriple],[RDFTriple])
getXPtriples' (xs,ys) | ys == [] = trace "getTriples' []" (xs,ys)
                      | p == armRes "hasTotalXP" = trace (show y) (y:xs',ys')
                      | p == armRes "addedXP" = trace (show y) (y:xs',ys')
                      | otherwise             = trace (show y) (xs',y:ys')
    where (xs',ys') = getXPtriples' (xs,tail ys)
          p = arcPred y
          y = head ys


xpSum :: [RDFTriple]  -- ^ Input list
      -> RDFTriple  -- ^ New arc
xpSum [] = error "xpSum called on empty list"
xpSum (x:[]) = arc (arcSubj x) (armRes "hasTotalXP") (arcObj x)
xpSum (x:y:xs) | arcSubj x /= arcSubj y = error "Subject mismatch in xpSum."
               | otherwise = xpSum (y':xs)
   where f = intFromRDF . arcObj
         t = f x + f y
         y' = arc (arcSubj x) p (litInt t)
         p = armRes "hasTotalXP"

