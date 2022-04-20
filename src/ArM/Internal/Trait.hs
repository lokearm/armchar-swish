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

-- | apply a given Trait Advancement to a given Trait
advanceTrait :: Trait -> Trait -> Trait 
advanceTrait trait adv = trait

makeNewTrait :: Trait -> Trait
makeNewTrait x = x

-- | Trait Resource
data Trait = Trait {
    traitID :: Maybe RDFLabel,
    traitClass :: Maybe String,
    traitContents :: [Triple]
   } deriving (Eq)

-- | Make a Trait object from a list of Quads
toTrait :: [Quad] -> Trait
toTrait [] = Trait {
         traitID = Nothing,
         traitClass = Nothing,
         traitContents = [] }
toTrait xs = Trait { 
         traitID = Just $ qfst $ head xs,
         traitClass = getTraitClass ys,
         traitContents = ys }
         where ys = toTripleList xs 

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

-- | Get the Trait Class from a list of Triples belonging to
-- an Trait Advancement
getTraitClass [] = Nothing
getTraitClass ((x,y,z):xs) 
   | y == "Trait ID"  = Just z
   | otherwise      = getTraitClass xs

