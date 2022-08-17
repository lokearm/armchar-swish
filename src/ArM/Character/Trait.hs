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
                           , kpToTrait
                           , kpToItem
                           , advanceTraitList
                           , advanceItemList
                           ) where

import           Data.Set (fromList)
import           Data.List (sort)
import           Swish.RDF.Graph 
import ArM.Resources
import ArM.KeyPair
import ArM.Types.Character

data XPType = XP { addXP :: Int, totalXP :: Int }
defaultXPType = XP { addXP = 0, totalXP = 0 }

-- |
-- = Trait Advancement

-- | Given one list of Traits and one of Trait advancements,
-- apply each advancement to the corresponding Trait.
-- The lists must be sorted by Trait class name.
advanceTraitList :: [Trait] -> [Trait] -> [Trait]
advanceTraitList xs [] = xs
advanceTraitList [] (y:ys) = recalculateXP y:advanceTraitList [] ys
advanceTraitList (x:xs) (y:ys) 
     | xc < yc  = x:advanceTraitList xs (y:ys)
     | xc > yc  = recalculateXP y:advanceTraitList (x:xs) ys
     | isRepeatableTrait x && x < y = x:advanceTraitList xs (y:ys)
     | isRepeatableTrait y = recalculateXP y:advanceTraitList (x:xs) ys
     | otherwise = advanceTraitList ( advanceTrait x y:xs ) ys
     where xc = traitClass x
           yc = traitClass y

-- | apply a given Trait Advancement to a given Trait
-- 1.  apply addedXP
-- 3.  take other properties from the second Trait if available
-- 4.  default to properties from the first Trait
advanceTrait :: Trait -> Trait -> Trait 
advanceTrait trait adv = recalculateXP  $
           trait { traitID = Nothing,
              traitContents = advanceTraitTriples 
                 ( traitContents trait ) 
                 ( traitContents adv ) 
           }

advanceTraitTriples :: [KeyValuePair] -> [KeyValuePair] -> [KeyValuePair]
advanceTraitTriples xs [] = xs
advanceTraitTriples [] ys = ys
advanceTraitTriples (x:xs) (y:ys) 
    | fst' x == fst' y   = y:advanceTraitTriples xs ys 
    | x < y   = x:advanceTraitTriples (xs) (y:ys) 
    | x > y   = y:advanceTraitTriples (x:xs) (ys) 
    where fst' (KeyValuePair a _) = a

-- |
-- == Recalculation of XP (auxiliary functions

-- | Make a new trait (for a CharacterSheet) from a Trait Advancement.
--  - add addedXP to totalXP (defaulting to 0)
--  - add implied traits
recalculateXP :: Trait -> Trait
recalculateXP x 
   | isXPTrait x  = x { traitID = Nothing,
        traitContents = makeNewTraitTriples $ traitContents x }
   | isAccelleratedTrait x  = x { traitID = Nothing,
        traitContents = makeNewTraitTriples $ traitContents x }
   | otherwise  = x

-- | Parse through the Triples of a Trait and recalculate score
-- based on XP
makeNewTraitTriples :: [KeyValuePair] -> [KeyValuePair]
makeNewTraitTriples ts = sort $ x:ys
    where (xp,ys) = makeNewTraitTriples' (defaultXPType,[]) ts
          KeyValuePair (totalXPLabel) (toRDFLabel ( totalXP xp + addXP xp ))

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

kpToTrait :: [KeyValuePair] -> Trait
kpToTrait [] = defaultTrait
kpToTrait xs = Trait { 
             traitID = k,
             traitClass = getTraitClass ys,
             isRepeatableTrait = a,
             isXPTrait = b,
             isAccelleratedTrait = c,
             traitContents = ys }
          where (k,a,b,c,ys) = traitKVList xs 

kpToItem :: KeyPairList -> Item
kpToItem (KeyPairList []) = defaultItem
kpToItem (KeyPairList xs) = Item { 
             itemID = getProperty prefixedidRes xs,
             itemClass = getTraitClass xs,
             itemLabel = getStringProperty (armRes "hasLabel") xs,
             itemContents = xs }

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

-- | Get the Trait Class from a list of Triples belonging to
-- an Trait Advancement
getTraitClass :: [KeyValuePair] -> RDFLabel
getTraitClass = f . getProperty ( armRes "traitClass" )
     where f Nothing = noSuchTrait
           f (Just x) = x

-- | Get the Item Class from a list of Triples belonging to
-- an Item Advancement
getItemClass :: [KeyValuePair] -> RDFLabel
getItemClass = f . getProperty ( armRes "itemClass" )
     where f Nothing = noSuchTrait
           f (Just x) = x

-- |
-- = Item Advancement 

-- | Given one list of Items and one of Item advancements,
-- apply each advancement to the corresponding Item.
-- The lists must be sorted by Item class name.
advanceItemList :: [Item] -> [Item] -> [Item]
advanceItemList xs [] = xs
advanceItemList [] ys = ys
advanceItemList (x:xs) (y:ys) 
     | x < y  = x:advanceItemList xs (y:ys)
     | x > y  = y:advanceItemList (x:xs) ys
     | otherwise = advanceItemList ( advanceItem x y:xs ) ys

-- | apply a given Item Advancement to a given Item
-- 3.  take other properties from the second Item if available
-- 4.  default to properties from the first Item
advanceItem :: Item -> Item -> Item 
advanceItem trait adv = 
           trait { itemID = Nothing,
              itemContents = advanceTraitTriples 
                 ( itemContents trait ) 
                 ( itemContents adv ) 
           }

