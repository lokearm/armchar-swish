-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Internal.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Auxiliary Functions to handle queries
--
-----------------------------------------------------------------------------
module ArM.Internal.Trait where

import Data.Set (fromList)

import Swish.RDF.Parser.N3 (parseN3fromText)
import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import qualified Data.Text.Lazy as T
import Swish.RDF.VarBinding as VB 
import Network.URI (URI)
import Swish.VarBinding  (vbMap)
import Data.Maybe
import Data.List (sort)
import ArM.Resources
import ArM.Query
import ArM.Metadata
import Swish.Namespace

-- ** The Data Type ** 
--
-- | Trait Resource
data Trait = Trait {
    traitID :: Maybe RDFLabel,
    traitClass :: Maybe RDFLabel,
    isRepeatableTrait :: Bool,
    isXPTrait :: Bool,
    isAccelleratedTrait :: Bool,
    traitContents :: [Triple]
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
         sc ((_,x,y):xs) = "  " ++ x ++ ": " ++ show y ++ "\n" ++ sc xs
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


advanceTraitTriples :: [Triple] -> [Triple] -> [Triple]
advanceTraitTriples (x:xs) (y:ys) 
    | fst' x == fst' y   = y:advanceTraitTriples xs ys 
    | x < y   = x:advanceTraitTriples (xs) (y:ys) 
    | x > y   = y:advanceTraitTriples (x:xs) (ys) 
    where fst' (a,b,c) = a


-- | Make a new trait (for a CharacterSheet) from a Trait Advancement.
--  - add addedXP to totalXP (defaulting to 0)
--  - add Score
--  - add implied traits
recalculateXP :: Trait -> Trait
recalculateXP x 
   | isXPTrait x  = x { traitID = Nothing,
        traitContents = makeNewTraitTriples 1 $ traitContents x }
   | isAccelleratedTrait x  = x { traitID = Nothing,
        traitContents = makeNewTraitTriples 5 $ traitContents x }
   | otherwise  = x

-- | Parse through the Triples of a Trait and recalculate score
-- based on XP
makeNewTraitTriples :: Int -> [Triple] -> [Triple]
makeNewTraitTriples n ts = sort $ x:y:z:ys
    where (xp,ys) = makeNewTraitTriples' (defaultXPType,[]) ts
          (x,y,z) = processXP n xp

-- | Parse through the Triples of a Trait and remove XP related traits
makeNewTraitTriples' :: (XPType,[Triple]) -> [Triple] -> (XPType,[Triple]) 
makeNewTraitTriples' xt [] = xt
makeNewTraitTriples' (xp,ys) ((a,b,c):zs) 
    | a == addXPLabel = makeNewTraitTriples' (xp { addXP = read c },ys) zs
    | a == totalXPLabel = makeNewTraitTriples' (xp { totalXP = read c },ys) zs
    | a == scoreLabel = makeNewTraitTriples' (xp { score = read c },ys) zs
    | a == hasXPLabel = makeNewTraitTriples' (xp { hasXP = read c },ys) zs
    | otherwise       = makeNewTraitTriples' (xp, (a,b,c):ys) zs
    where read = fromJust . fromRDFLabel


-- | Calculate the triples for total XP, score, and remaining XP,
-- given an XPType object.
processXP :: Int -> XPType -> (Triple,Triple,Triple)
processXP n xp = ( (totalXPLabel,"Total XP",toRDFLabel t),
                 (scoreLabel,"Score",toRDFLabel s),
                 (hasXPLabel,"XP towards next level",toRDFLabel r) ) 
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
toTrait :: [Quad] -> Trait
toTrait [] = Trait {
         traitID = Nothing,
         traitClass = Nothing,
         isRepeatableTrait = False,
         isXPTrait = False,
         isAccelleratedTrait = False,
         traitContents = [] }
toTrait xs = Trait { 
         traitID = Just $ qfst $ head xs,
         traitClass = getTraitClass ys,
         isRepeatableTrait = a,
         isXPTrait = b,
         isAccelleratedTrait = c,
         traitContents = ys }
         where (a,b,c,ys) = traitTripleList xs 

-- | Remove the first element from each Quad in a list
traitTripleList :: [Quad] -> (Bool,Bool,Bool,[Triple])
traitTripleList xs = traitTripleList' (False,False,False,[]) xs
traitTripleList' :: (Bool,Bool,Bool,[Triple]) -> [Quad]  
                 -> (Bool,Bool,Bool,[Triple])
traitTripleList' (a,b,c,xs) [] = (a,b,c,xs)
traitTripleList' (a,b,c,xs) (y:ys) =
         traitTripleList' (a',b',c',(y2,y3,y4):xs) ys
         where  a' = a || y4 == repeatableLabel
                b' = b || y4 == xptraitLabel
                c' = c || y4 == accelleratedtraitLabel
                (_,y2,y3,y4) = y

-- | Get the Trait Class from a list of Triples belonging to
-- an Trait Advancement
getTraitClass [] = Nothing
getTraitClass ((x,y,z):xs) 
   | y == "Trait ID"  = Just z
   | otherwise      = getTraitClass xs

traitToArcSet :: Trait -> RDFArcSet
traitToArcSet = fromList . traitToArcList
traitToArcList :: Trait -> [RDFTriple]
-- traitToArcList t | x == Nothing = triplesToArcList y ts
traitToArcList t | otherwise    = triplesToArcList (fromJust x) ts
                 where y = Blank "foobar"
                       x = traitID t
                       ts = traitContents t
triplesToArcList :: RDFLabel -> [Triple] -> [RDFTriple]
triplesToArcList x [] = []
triplesToArcList x ((a,b,c):ys) = arc x a c:triplesToArcList x ys

