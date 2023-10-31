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
                       , traitID
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
    instanceLabel :: String,
    isRepeatableTrait :: Bool,
    traitContents :: [RDFTriple]
   } deriving (Eq)
defaultTrait :: Trait
defaultTrait = Trait {
    traitClass = noSuchTrait,
    instanceLabel = "",
    isRepeatableTrait = False,
    traitContents = []
   } 

-- | Get the ID (RDFLabel) of a trait if possible.
traitID :: Trait -> Maybe RDFLabel
traitID = f . traitContents
   where f [] = Nothing
         f (x:_) = Just $ arcSubj x

instance Show Trait where
   show a = "**" ++ show (traitClass a) ++ "**\n" 
                 ++ sc (traitContents a) 
                 ++ "\n"
      where 
         sc [] = ""
         sc (x:xs) = "  " ++ show x ++ "\n" ++ sc xs
instance Ord Trait where
   compare x y | traitClass x < traitClass y = LT
               | traitClass x > traitClass y = GT
               -- | not (isRepeatableTrait x) = EQ
               -- | not (isRepeatableTrait y) = EQ
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

--

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
advanceTrait trait adv = trace ("advanceTrait " ++ tid trait ++ " " ++ tid adv) $
  fixTrait $ trait { traitContents = map fixSubj 
           $ advanceTriples ( traitContents trait ) 
                            ( sort $ traitContents adv ) }
      where fixSubj x = arc ( armRes "unnamedBlankNode" ) 
                            ( arcPred x ) ( arcObj x )
            tid = show . traitID

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
         makeXParc [] zs = zs
         makeXParc ws zs = getXParc ws:zs

-- | Auxiliary for `calculateXP`
getXParc :: [RDFTriple] -> RDFTriple
getXParc [] = error "getXParc should not be called on an empty list"
getXParc (x:xs) = trace ("getXParc "++show xp) $
                  arc (arcSubj x) (armRes "hasTotalXP") (litInt xp)
   where xp = foldr (+) 0 $ map ( intFromRDF . arcObj ) (x:xs)

-- | Auxiliary for `calculateXP`
getXPtriples :: [RDFTriple] -> ([RDFTriple],[RDFTriple])
getXPtriples xs = trace "getXPtriples" $ getXPtriples' ([],xs)

-- | Auxiliary for `calculateXP`
getXPtriples' :: ([RDFTriple],[RDFTriple]) -> ([RDFTriple],[RDFTriple])
getXPtriples' (xs,ys) | ys == [] = (xs,ys)
                      | p == armRes "hasTotalXP" =  (y:xs',ys')
                      | p == armRes "addedXP" =  (y:xs',ys')
                      | otherwise             =  (xs',y:ys')
    where (xs',ys') = getXPtriples' (xs,tail ys)
          p = arcPred y
          y = head ys

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
