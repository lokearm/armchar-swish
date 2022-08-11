{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Auxiliary Functions to handle queries.
-- When parsing a trait without an arm:traitClass property, Nothing
-- is returned.  Thus such traits will be discarded.  
--
-----------------------------------------------------------------------------
module ArM.Character.Trait ( Trait(..)
                           , Item(..)
                           , defaultTrait
                           , kpToTrait
                           , kpToItem
                           , advanceTraitList
                           , advanceItemList
                           ) where

import Data.Set (fromList)

import           Swish.RDF.Graph as G
import           Network.URI (URI)
import           Swish.VarBinding  (vbMap)
import           Data.List (sort)
import ArM.Resources
import ArM.KeyPair
import ArM.BlankNode
import ArM.Types.Character

data XPType = XP { addXP :: Int, totalXP :: Int, score :: Int, hasXP :: Int }
defaultXPType = XP { addXP = 0, totalXP = 0, score = 0, hasXP = 0 }

-- ** Advancement **

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
-- 2.  recalculate Score
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


-- | Make a new trait (for a CharacterSheet) from a Trait Advancement.
--  - add addedXP to totalXP (defaulting to 0)
--  - add Score
--  - add implied traits
recalculateXP :: Trait -> Trait
recalculateXP x 
   | isXPTrait x  = x { traitID = Nothing,
        traitContents = makeNewTraitTriples 5 $ traitContents x }
   | isAccelleratedTrait x  = x { traitID = Nothing,
        traitContents = makeNewTraitTriples 1 $ traitContents x }
   | otherwise  = x

-- | Parse through the Triples of a Trait and recalculate score
-- based on XP
makeNewTraitTriples :: Int -> [KeyValuePair] -> [KeyValuePair]
makeNewTraitTriples n ts = sort $ x:y:z:ys
    where (xp,ys) = makeNewTraitTriples' (defaultXPType,[]) ts
          (x,y,z) = processXP n xp

-- | Parse through the Triples of a Trait and remove XP related traits
makeNewTraitTriples' :: (XPType,[KeyValuePair]) -> [KeyValuePair] -> (XPType,[KeyValuePair]) 
makeNewTraitTriples' xt [] = xt
makeNewTraitTriples' (xp,ys) (KeyValuePair a c:zs) 
    | a == addXPLabel = makeNewTraitTriples' (xp { addXP = read c },ys) zs
    | a == totalXPLabel = makeNewTraitTriples' (xp { totalXP = read c },ys) zs
    | a == scoreLabel = makeNewTraitTriples' (xp { score = read c },ys) zs
    | a == hasXPLabel = makeNewTraitTriples' (xp { hasXP = read c },ys) zs
    | otherwise       = makeNewTraitTriples' (xp, KeyValuePair a c:ys) zs
    where read = f . fromRDFLabel
          f Nothing = 0
          f (Just x) = x


-- | Calculate the triples for total XP, score, and remaining XP,
-- given an XPType object.
processXP :: Int -> XPType -> (KeyValuePair,KeyValuePair,KeyValuePair)
processXP n xp = ( KeyValuePair (totalXPLabel) (toRDFLabel t),
                 KeyValuePair (scoreLabel) (toRDFLabel s),
                 KeyValuePair (hasXPLabel) (toRDFLabel r) ) 
   where t = totalXP xp + addXP xp
         s = scoreFromXP (t `div` n)
         r = t - n*(s*(s+1) `div` 2)

-- | Calculate score from total XP, using the arts scale.
-- For abilities, the argument should be divided by 5 beforehand.
scoreFromXP :: Int -> Int
scoreFromXP y = floor $ (-1+sqrt (1+8*x))/2
    where x = fromIntegral y  :: Double


-- ** Parsing Trait from RDF **

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

-- ** Item Advancement **

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

