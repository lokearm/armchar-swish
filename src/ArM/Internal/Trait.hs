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
    traitClass :: Maybe String,
    isRepeatableTrait :: Bool,
    isXPTrait :: Bool,
    isAccelleratedTrait :: Bool,
    traitContents :: [Triple]
   } deriving (Eq)

instance Show Trait where
   show a = "**" ++ y (traitID a) ++ " " ++ s (traitClass a) ++ "**\n" 
                 ++ sc (traitContents a) 
                 ++ "\n"
      where 
         y Nothing = ""
         y (Just x) = show x
         s Nothing = ""
         s (Just x) = x
         sc [] = ""
         sc ((_,x,y):xs) = "  " ++ x ++ ": " ++ y ++ "\n" ++ sc xs
instance Ord Trait where
   compare x y | traitClass x < traitClass y = LT
               | traitClass x > traitClass y = GT
               | traitID x < traitID y = LT
               | traitID x > traitID y = GT
               | otherwise = EQ

data XPType = XP { addXP :: Int, totalXP :: Int, score :: Int, hasXP :: Int }
defaultXPType = XP { addXP = 0, totalXP = 0, score = 0, hasXP = 0 }

-- ** Advancement **

advanceTraitList :: [Trait] -> [Trait] -> [Trait]
advanceTraitList xs [] = xs
advanceTraitList [] (y:ys) = makeNewTrait y:advanceTraitList [] ys
advanceTraitList (x:xs) (y:ys) 
     | xc < yc  = x:advanceTraitList xs (y:ys)
     | xc > yc  = makeNewTrait y:advanceTraitList (x:xs) ys
     | isRepeatableTrait x && x < y = x:advanceTraitList xs (y:ys)
     | isRepeatableTrait y = makeNewTrait y:advanceTraitList (x:xs) ys
     | otherwise = advanceTraitList ( advanceTrait x y:xs ) ys
     where xc = traitClass x
           yc = traitClass y

-- | apply a given Trait Advancement to a given Trait
-- 1.  apply addedXP
-- 2.  recalculate Score
-- 3.  take other properties from the second Trait if available
-- 4.  default to properties from the first Trait
advanceTrait :: Trait -> Trait -> Trait 
advanceTrait trait adv = trait { traitID = Nothing }

-- | Make a new trait (for a CharacterSheet) from a Trait Advancement.
--  - replace addedXP with totalXP 
--  - add Score
--  - add implied traits
makeNewTrait :: Trait -> Trait
makeNewTrait x = x { traitID = Nothing,
        traitContents = makeNewTraitTriples $ traitContents x }

makeNewTraitTriples :: [Triple] -> [Triple]
makeNewTraitTriples ts = x:y:z:ys
    where (xp,ys) = makeNewTraitTriples' (defaultXPType,[]) ts
          (x,y,z) = processXP xp

processXP :: XPType -> (Triple,Triple,Triple)
processXP xp = ( (totalXPLabel,"Total XP",show t),
                 (scoreLabel,"Score",show s),
                 (hasXPLabel,"XP towards next level",show r) ) 
   where t = totalXP xp + addXP xp
         s = scoreFromXP t
         r = t - (s*(s+1) `div` 2)
scoreFromXP :: Int -> Int
scoreFromXP y = floor $ (-1+sqrt (1+8*x))/2
    where x = fromIntegral y  :: Double

addXPLabel = Res $ makeSN  "addedXP"
totalXPLabel = Res $ makeSN "hasTotalXP" 
scoreLabel = Res $ makeSN "hasScore" 
hasXPLabel = Res $ makeSN "hasXP" 

-- ** Parsing Trait from RDF **

makeNewTraitTriples' :: (XPType,[Triple]) -> [Triple] -> (XPType,[Triple]) 
makeNewTraitTriples' xt [] = xt
makeNewTraitTriples' (xp,ys) ((a,b,c):zs) 
    | a == addXPLabel = makeNewTraitTriples' (xp { addXP = read c },ys) zs
    | a == totalXPLabel = makeNewTraitTriples' (xp { totalXP = read c },ys) zs
    | a == scoreLabel = makeNewTraitTriples' (xp { score = read c },ys) zs
    | a == hasXPLabel = makeNewTraitTriples' (xp { hasXP = read c },ys) zs
    | otherwise       = makeNewTraitTriples' (xp, (a,b,c):ys) zs

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
traitTripleList' :: (Bool,Bool,Bool,[Triple]) -> [Quad] -> (Bool,Bool,Bool,[Triple])
traitTripleList' (a,b,c,[]) ys = (a,b,c,[]) 
traitTripleList' (a,b,c,xs) (y:ys) =
         traitTripleList' (a',b',c',(y2,y3,y4):xs) ys
         where  a' = a || y4 == "arm:RepeatableTrait"
                b' = b || y4 == "arm:XPTrait"
                c' = c || y4 == "arm:AccelleratedTrait"
                (_,y2,y3,y4) = y

-- | Get the Trait Class from a list of Triples belonging to
-- an Trait Advancement
getTraitClass [] = Nothing
getTraitClass ((x,y,z):xs) 
   | y == "Trait ID"  = Just z
   | otherwise      = getTraitClass xs

