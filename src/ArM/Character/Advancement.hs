{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Handling character advancement
--
-----------------------------------------------------------------------------

module ArM.Character.Advancement ( Advancement(..)
                                 , getPregameAdvancements
                                 , getIngameAdvancements
                                 , defaultAdvancement
                                 , getSeason
                                 , getYear
                                 , getSortIndex
                                 ) where

import Swish.RDF.Graph 
import Swish.RDF.Query as Q
import ArM.Resources 
import ArM.KeyPair 
import Data.Maybe 
import Data.List
import ArM.Character.Trait
import ArM.Types.Character
import ArM.Rules.Aux
import qualified Swish.RDF.VarBinding  as VB
import           Swish.VarBinding  (vbMap)

-- Class:
--    a arm:CharacterAdvancement ;
-- Time:
--    arm:atSeasonTime arm:summer1217 ;
-- Character?
--    arm:advanceToCharacterSheet :autumn1217
-- Descriptive:
--    arm:hasAdvancementDescription "Studies Herbam L6 Q21 +3" ;
--    arm:awardsXP 21 ;
--    arm:hasAdvancementType arm:Reading ;
-- Traits (multiple)
--    arm:advanceTrait [ a armr:herbam ; arm:addedXP 21 ] ;


-- | Get a list of all Pregame Advancements of a character.
getPregameAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getPregameAdvancements g c = getAdvancements g $ queryGraph preGameAdv c
   where preGameAdv = armRes  "PregameAdvancement"

-- | Get a list of all Ingame Advancements of a character.
getIngameAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getIngameAdvancements g c = getAdvancements g $ queryGraph inGameAdv c
   where inGameAdv = armRes  "IngameAdvancement"

-- | Query graph to find a advancements of a given type (RDF class)
-- for a given character.
queryGraph :: RDFLabel -- ^ Label for the advancement type
           -> RDFLabel -- ^ Label for the character to be advanced
           -> RDFGraph -- ^ Resulting graph
queryGraph c1 = listToRDFGraph  . f c1
   where f c1 c2 = [ arc (Var "id") typeRes c1,
            arc (Var "id") (Var "property") (Var "value"),
            arc (Var "id") (armRes  "advanceCharacter") c2,
            arc (Var "property") labelRes (Var "label") ]

-- | Generic version of 'getIngameAdvancements' and 'getPregameAdvancements'
getAdvancements :: RDFGraph -> RDFGraph -> [Advancement]
getAdvancements g = fixAdvancements g . 
               map toAdvancement . arcListSplit . getGenQuads g 

-- | Auxiliary for 'getAdvancements'
getGenQuads :: RDFGraph -> RDFGraph -> [RDFTriple]
getGenQuads g q = map arcFromBinding $ rdfQueryFind q g

-- | Auxiliary for `getAdvancements`
fixAdvancements :: RDFGraph -> [Advancement] -> [Advancement]
fixAdvancements g adv = map (fixAdv g) adv

-- | Auxiliary for 'fixAdvancements'
fixAdv :: RDFGraph -> Advancement -> Advancement
fixAdv g adv = adv { traits = traitsFromRDF advid g,
                 items = itemsFromRDF advid g }
        where advid = rdfid adv

itemsFromRDF advid g = itFromRDF "changePossession" advid g
traitsFromRDF advid g = itFromRDF "advanceTrait" advid g

itFromRDF s advid g = splitTrait $ sort $ map vb2tt $ rdfQueryFind q g 
    where q = traitqgraph (armRes s) advid

vb2tt :: VB.RDFVarBinding -> Trait
vb2tt vb = defaultTrait { traitClass = fromJust $ vbMap vb (Var "class"),
               traitContents = [ arc (fromJust $ vbMap vb (Var "id")) 
                               (fromJust $ vbMap vb (Var "property"))
                               (fromJust $ vbMap vb (Var "value")) ] }

splitTrait :: [Trait] -> [Trait]
splitTrait xs = fst $ splitTrait' ([],xs)
splitTrait' :: ([Trait],[Trait]) -> ([Trait],[Trait])
splitTrait' (ts,[]) = (ts,[])
splitTrait' ([],x:xs) = splitTrait' (x:[],xs) 
splitTrait' (t:ts,x:xs) 
    | traitClass t == traitClass x = splitTrait' (t':ts,xs) 
    | otherwise                    = splitTrait' (x:t:ts,xs) 
       where t' = addToTrait x t

addToTrait :: Trait -> Trait -> Trait
addToTrait t x | traitClass t /= traitClass x 
                      = error "traitClass mismatch in addToTrait"
      | otherwise = t { traitContents = traitContents t ++ traitContents x }

traitqgraph :: RDFLabel -> RDFLabel -> RDFGraph
traitqgraph p s = listToRDFGraph 
      [ arc s p (Var "id")
      , arc (Var "id") (Var "property") (Var "value")
      , arc (Var "id") (armRes "traitClass") (Var "class") ]

-- | Make an Advancement object from a list of Quads
toAdvancement :: [RDFTriple] -> Advancement
toAdvancement [] = defaultAdvancement 
toAdvancement xs = defaultAdvancement { rdfid = getkey xs,
         year = getYear ys,
         season = getSeason ys,
         advSortIndex = getSortIndex ys,
         contents = ys }
         where ys = toKeyPairList xs 
               getkey [] = noSuchAdvancement
               getkey (x:xs) = arcSubj x

-- | Get the year from a list of Triples belonging to an Advancement
getYear :: [KeyValuePair] -> Maybe Int
getYear xs = f1 x
     where  x = getProperty inYear xs
            f1 Nothing = Nothing
            f1 (Just year) = fromRDFLabel year

-- | Get the season from a list of Triples belonging to an Advancement
getSeason = getStringProperty atSeason

-- | Get sort index from a list of Triples belonging to an Advancement
getSortIndex :: [KeyValuePair] -> Int
getSortIndex xs = f1 x
     where  x = getProperty hasAdvancementIndex xs
            f1 Nothing = 2^30
            f1 (Just idx) = f2 $ fromRDFLabel idx
            f2 Nothing = 2^30
            f2 (Just idx) = idx

instance FromRDFGraph Advancement where
   fromRDFGraph g label = fixAdv g $ defaultAdvancement {
                 rdfid = label,
                 year = getYear ys,
                 season = getSeason ys,
                 advSortIndex = getSortIndex ys,
                 contents = ys }
        where q = listToRDFGraph  $
                  [ arc label typeRes advancementType,
                    arc label (Var "property") (Var "value"),
                    arc (Var "property") typeRes armViewProperty,
                    arc (Var "property") labelRes (Var "label") ]
              vb = Q.rdfQueryFind g q
              ys = map keypairFromBinding vb
