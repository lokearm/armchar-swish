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
-- The advancement logic depends on Haskell types rather than
-- working on the RDFGraph.  It is possible that this is slower
-- than necessary.
-- It is important to separate the different advancements into 
-- separata data structures, sort them, and apply them in order.
-- Working with all the advancement in one RDFGraph is expensive.
-- However, the parsing into Haskell types may not be necessary.
--
-----------------------------------------------------------------------------
module ArM.Types.Trait ( Trait(..)
                       , defaultTrait
                       , advanceTraitList
                       ) where

import Swish.RDF.Graph as G
import Data.List (sort)
import Data.Aeson
import ArM.KeyPair
import ArM.Resources
import ArM.Rules.Aux
import ArM.Types.RDF()

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
data Trait = Trait {
    traitClass :: RDFLabel,
    traitID :: RDFLabel,
    instanceLabel :: String,
    isRepeatableTrait :: Bool,
    traitContents :: [RDFTriple]
   } deriving (Eq)
defaultTrait :: Trait
defaultTrait = Trait {
    traitClass = noSuchTrait,
    traitID = noSuchTrait,
    instanceLabel = "",
    isRepeatableTrait = False,
    traitContents = []
   } 

instance Show Trait where
   show a = "**" ++ show (traitClass a) ++ "** (" ++ show (instanceLabel a) ++")\n" 
                 ++ sc (traitContents a) 
                 ++ "\n"
      where 
         sc [] = ""
         sc (x:xs) = "  " ++ show x ++ "\n" ++ sc xs
instance Ord Trait where
   compare x y | traitClass x < traitClass y = LT
               | traitClass x > traitClass y = GT
               | instanceLabel x < instanceLabel y = LT
               | instanceLabel x > instanceLabel y = GT
               | otherwise = EQ

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
fixTrait trait =  trait {
       traitContents = sort $ calculateXP $ traitContents trait
    }

-- |
-- == Recalculation of XP (auxiliary functions

-- | Auxiliary for `fixTrait`.
--
calculateXP :: [RDFTriple] -> [RDFTriple]
calculateXP ts = trace ("calculateXP" ++ show (tot,add,fac)) $ xp:ys 
   where (tot,add,fac,ys) = getXPtriples' (0,0,1,ts)
         newtot = round $ (fromIntegral tot) + (fromIntegral add)*fac
         sub = arcSubj $ head ts
         xp = arc sub (armRes "hasTotalXP") (litInt newtot)

-- | Inner recursive function for `getXPtriplles` (auxiliary for `calculateXP`)
getXPtriples' :: (Int,Int,Float,[RDFTriple]) -> (Int,Int,Float,[RDFTriple])
getXPtriples' (tot,add,fac,ys) | ys == [] = (tot,add,fac,ys)
                      | p == armRes "hasTotalXP" =  (newtot,add',fac',ys')
                      | p == armRes "addedXP" =  (tot',newadd,fac',ys')
                      | p == armRes "hasXPfactor" =
                         trace (show (tot',add',newfac) )
                         (tot',add',newfac,y:ys')
                      | otherwise             =  (tot',add',fac',y:ys')
    where (tot',add',fac',ys') = getXPtriples' (tot,add,fac,tail ys)
          p = arcPred y
          newfac = floatFromRDF $ arcObj y
          newtot = tot' + ( intFromRDF $ arcObj y )
          newadd = add' + ( intFromRDF $ arcObj y )
          y = ttrace $ head ys

-- | Auxiliary for `fixTrait`.
--
calculateQ :: [RDFTriple] -> [RDFTriple]
calculateQ ts = trace ("calculateQ" ++ show (tot,add)) $ xp:ys 
   where (tot,add,ys) = getQtriples' (0,0,ts)
         newtot = (fromIntegral tot) + (fromIntegral add)
         sub = arcSubj $ head ts
         xp = arc sub (armRes "hasTotalXP") (litInt newtot)

-- | Inner recursive function for `getQtriplles` (auxiliary for `calculateQ`)
getQtriples' :: (Int,Int,[RDFTriple]) -> (Int,Int,[RDFTriple])
getQtriples' (tot,add,ys) | ys == [] = (tot,add,ys)
                      | p == armRes "hasQuantity" =  (newtot,add',ys')
                      | p == armRes "addQuantity" =  (tot',newadd,ys')
                      | p == armRes "removeQuantity" =  (tot',newrm,ys')
                      | otherwise             =  (tot',add',y:ys')
    where (tot',add',ys') = getQtriples' (tot,add,tail ys)
          p = arcPred y
          newtot = tot' + ( intFromRDF $ arcObj y )
          newadd = add' + ( intFromRDF $ arcObj y )
          newrm = add' - ( intFromRDF $ arcObj y )
          y = ttrace $ head ys

{-
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
-}
