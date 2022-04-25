{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Auxiliary Functions to handle queries
--
-----------------------------------------------------------------------------
module ArM.Character.Trait where

import Data.Set (fromList)

import ArM.Rules.Aux
import Swish.RDF.Parser.N3 (parseN3fromText)
import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import qualified Data.Text.Lazy as T
import Swish.RDF.VarBinding as VB 
import Network.URI (URI)
import Swish.VarBinding  (vbMap)
import Data.Maybe (fromJust)
import Data.List (sort)
import ArM.Resources
import ArM.KeyPair
import ArM.BlankNode
import Swish.Namespace

-- ** The Data Type ** 

-- | Trait Resource
-- `traitID` and `traitContents` are sufficient to describe the trait.
-- The other fields duplicate information to facilitate searching and
-- sorting.
-- When new traits are created, `traitID` is set to nothing?
-- A blank node is only created when it is written into an RDFGraph.
data Trait = Trait {
    traitID :: Maybe RDFLabel,
    traitClass :: Maybe RDFLabel,
    isRepeatableTrait :: Bool,
    isXPTrait :: Bool,
    isAccelleratedTrait :: Bool,
    traitContents :: [KeyValuePair]
   } deriving (Eq)

instance Show Trait where
   show a = "**" ++ y (traitID a) ++ " " ++ y (traitClass a) ++ "**\n" 
                 ++ sc (traitContents a) 
                 ++ "\n"
      where 
         y Nothing = ""
         y (Just x) = show x
         s Nothing = ""
         s (Just x) = x
         sc [] = ""
         sc (KeyValuePair x y:xs) = "  " ++ show x ++ ": " ++ show y ++ "\n" ++ sc xs
instance Ord Trait where
   compare x y | traitClass x < traitClass y = LT
               | traitClass x > traitClass y = GT
               | traitID x < traitID y = LT
               | traitID x > traitID y = GT
               | otherwise = EQ

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

-- | Make a Trait object from a list of Quads
toTrait :: [RDFTriple] -> Trait
toTrait [] = Trait {
         traitID = Nothing,
         traitClass = Nothing,
         isRepeatableTrait = False,
         isXPTrait = False,
         isAccelleratedTrait = False,
         traitContents = [] }
toTrait xs = Trait { 
         traitID = Just $ arcSubj $ head xs,
         traitClass = getTraitClass ys,
         isRepeatableTrait = a,
         isXPTrait = b,
         isAccelleratedTrait = c,
         traitContents = ys }
         where (a,b,c,ys) = traitTripleList xs 

-- | Remove the first element from each Quad in a list
traitTripleList :: [RDFTriple] -> (Bool,Bool,Bool,[KeyValuePair])
traitTripleList xs = traitTripleList' (False,False,False,[]) xs
traitTripleList' :: (Bool,Bool,Bool,[KeyValuePair]) -> [RDFTriple]  
                 -> (Bool,Bool,Bool,[KeyValuePair])
traitTripleList' (a,b,c,xs) [] = (a,b,c,xs)
traitTripleList' (a,b,c,xs) (y:ys) =
         traitTripleList' (a',b',c',KeyValuePair y2 y4:xs) ys
         where  a' = a || y4 == repeatableLabel
                b' = b || y4 == xptraitLabel
                c' = c || y4 == accelleratedtraitLabel
                y2 = arcPred y
                y4 = arcObj y

-- | Get the Trait Class from a list of Triples belonging to
-- an Trait Advancement
getTraitClass [] = Nothing
getTraitClass (KeyValuePair y z:xs) 
   | y == "Trait ID"  = Just z
   | otherwise      = getTraitClass xs
-- TODO  The check needs to use RDFLabel .


triplesToArcList :: RDFLabel -> [KeyValuePair] -> [RDFTriple]
triplesToArcList x [] = []
triplesToArcList x (KeyValuePair a c:ys) = arc x a c:triplesToArcList x ys

traitToArcListM :: RDFLabel -> Trait -> BlankState [RDFTriple]
traitToArcListM cs t 
     | x' == Nothing = do
                      y <- getBlank 
                      return $ arc cs htRes y:triplesToArcList y ts
     | otherwise    = return $ arc cs htRes x:triplesToArcList x ts
                 where x' = traitID t
                       x = fromJust x'
                       ts = traitContents t

