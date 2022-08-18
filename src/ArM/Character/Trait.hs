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


-- |
-- = Trait Advancement

-- | Given one list of Traits and one of Trait advancements,
-- apply each advancement to the corresponding Trait.
-- The lists must be sorted by Trait class name.
advanceTraitList :: [Trait] -> [Trait] -> [Trait]
advanceTraitList xs [] = xs
advanceTraitList [] (y:ys) = y:advanceTraitList [] ys
advanceTraitList (x:xs) (y:ys) 
  | x < y  = x:advanceTraitList xs (y:ys)
  | x > y  = y:advanceTraitList (x:xs) ys
  | otherwise = advanceTraitList ( (advanceTrait x y):xs ) ys
     where xc = traitClass x
           yc = traitClass y

-- | apply a given Trait Advancement to a given Trait
-- 1.  take other properties from the second Trait if available
-- 2.  default to properties from the first Trait
advanceTrait :: Trait -> Trait -> Trait 
advanceTrait trait adv = trait { traitContents = advanceTriples 
                                             ( traitContents trait ) 
                                             ( traitContents adv ) 
                               }

-- | Merge two lists of trait statements using `advanceTriple1`.
-- Then total XP is recalculated adding up all `hasTotalXP` and
-- `addedXP` properties.
advanceTriples :: [RDFTriple] -> [RDFTriple] -> [RDFTriple]
advanceTriples x = sort . map fixSubj 
                 . advanceTriples2 . advanceTriples1 x

fixSubj :: RDFTriple -> RDFTriple
fixSubj x = arc ( armRes "unnamedBlankNode" ) ( arcPred x ) ( arcObj x )


-- | Merge two lists of trait statements.  If a subject/property
-- pair is found in both lists, it is taken only from the former.
advanceTriples1 :: [RDFTriple] -> [RDFTriple] -> [RDFTriple]
advanceTriples1 xs [] = xs
advanceTriples1 [] ys = ys
advanceTriples1 (x:xs) (y:ys) 
    | arcSubj x /=  arcSubj y = error "Conflicting Trait IDs in advanceTriples1."
    | arcPred x < arcPred y = x:advanceTriples (xs) (y:ys)
    | arcPred x > arcPred y = y:advanceTriples (x:xs) (ys)
    | otherwise = x:advanceTriples xs ys

advanceTriples2 :: [RDFTriple] -> [RDFTriple]
advanceTriples2 xs = makeXParc xs ys 
   where (xs,ys) = getXPtriples xs
makeXParc [] ys = ys
makeXParc xs ys = getXParc xs:ys

-- | TODO Dummy
getXParc (x:xs) = x

getXPtriples :: [RDFTriple] -> ([RDFTriple],[RDFTriple])
getXPtriples xs = getXPtriples' ([],xs)
getXPtriples' :: ([RDFTriple],[RDFTriple]) -> ([RDFTriple],[RDFTriple])
getXPtriples' (xs,ys) | ys == [] = (xs,ys)
                      | p == armRes "hasTotalXP" = (y:xs',ys')
                      | p == armRes "addedXP" = (y:xs',ys')
                      | otherwise             = (xs',y:ys')
    where (xs',ys') = getXPtriples' (xs,yt)
          p = arcPred y
          y = head ys
          yt = tail ys

-- |
-- == Recalculation of XP (auxiliary functions

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

