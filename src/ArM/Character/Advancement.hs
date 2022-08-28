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
                                 , getAllAdvancements
                                 , defaultAdvancement
                                 , getSeason
                                 , getYear
                                 , getSortIndex
                                 , parseTime
                                 ) where

import Swish.RDF.Graph 
import Swish.RDF.Query as Q
import ArM.Resources 
import ArM.KeyPair 
import Data.Maybe 
import Data.List
import ArM.Character.Trait
import ArM.Types.Character
import ArM.Types.Season
import ArM.Rules.Aux
import qualified Swish.RDF.VarBinding  as VB
import           Swish.VarBinding  (vbMap)

-- import Debug.Trace
trace x y = y

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

getAllAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getAllAdvancements g c = getAdvancements g $ queryGraph inGameAdv c
   where inGameAdv = armRes  "CharacterAdvancement"

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
fixAdv g adv = trace ("fixAdv "++show advid) $ adv { traits = traitsFromRDF advid g,
                 items = itemsFromRDF advid g }
        where advid = rdfid adv

itemsFromRDF advid g = itFromRDF True "changePossession" advid g
traitsFromRDF advid g = itFromRDF False "advanceTrait" advid g


itFromRDF :: Bool -> String -> RDFLabel -> RDFGraph -> [Trait]
itFromRDF b s advid g = splitTrait $ sort $ map (vb2tt b) $ rdfQueryFind q g 
    where q = traitqgraph (armRes s) advid

type ProtoTrait = (RDFLabel, RDFLabel, RDFLabel,RDFLabel)
vb2tt :: Bool -> VB.RDFVarBinding -> ProtoTrait
vb2tt b vb = ( fromJust $ vbMap vb (Var "class")
             , (fromJust $ vbMap vb (Var "id")) 
             , (fromJust $ vbMap vb (Var "property"))
             , (fromJust $ vbMap vb (Var "value")) 
             )

splitTrait :: [ProtoTrait] -> [Trait]
splitTrait xs = fst $ splitTrait' ([],xs)
splitTrait' :: ([Trait],[ProtoTrait]) -> ([Trait],[ProtoTrait])
splitTrait' (ts,[]) = (ts,[])
splitTrait' ([],x:xs) = splitTrait' (mkTrait x:[],xs) 
splitTrait' (t:ts,x:xs) 
    | traitClass t == c = splitTrait' (t':ts,xs) 
    | otherwise         = splitTrait' (mkTrait x:t:ts,xs) 
       where t' = addToTrait t x
             (c,s,p,o) = x
mkTrait :: ProtoTrait -> Trait
mkTrait (a,b,c,d) = defaultTrait { traitClass = a,
                         traitContents = [ arc b c d ] }


addToTrait :: Trait -> ProtoTrait -> Trait
addToTrait t (c,s,p,o) 
      | traitClass t /= c = error "traitClass mismatch in addToTrait"
      | p == armRes "instanceLabel" 
             = t { instanceLabel = lab o
                 , traitContents = triple:traitContents t }
      | p == typeRes && o == armRes "RepeatableTrait" 
                        = t { isRepeatableTrait = True
                            , traitContents = triple:traitContents t }
      | p == typeRes && o == armRes "Equipment" 
                        = t { isRepeatableTrait = True
                            , traitContents = triple:traitContents t }
      | otherwise = t { traitContents = triple:traitContents t }
         where lab = f . rdfToString 
               triple = arc s p o
               f Nothing = ""
               f (Just x) = x

traitqgraph :: RDFLabel -> RDFLabel -> RDFGraph
traitqgraph p s = listToRDFGraph 
      [ arc s p (Var "id")
      , arc (Var "id") (Var "property") (Var "value")
      , arc (Var "id") (armRes "traitClass") (Var "class") ]

-- | Make an Advancement object from a list of Quads
toAdvancement :: [RDFTriple] -> Advancement
toAdvancement xs = defaultAdvancement { rdfid = getkey xs
                                      , advTime = parseTime defaultCharTime ys 
                                      , contents = ys }
         where ys = toKeyPairList xs 
               getkey [] = noSuchAdvancement
               getkey (x:xs) = arcSubj x

parseTime :: CharTime  -> [KeyValuePair] -> CharTime
parseTime a [] = a
parseTime a (x:xs) = parseTime (f a x) xs
  where f a (KeyValuePair k v) 
         | k == inYear = a { charYear = rdfToInt v }
         | k == atSeason = a { charSeason = fs (rdfToString v) }
         | k == hasAdvancementIndex = a { advancementIndex = fi (rdfToInt v) }
         | otherwise = a
         where fs Nothing = "" 
               fs (Just x) = x
               fi Nothing = 0 
               fi (Just x) = x

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
   fromRDFGraph g label = fixAdv g $ defaultAdvancement 
                 { rdfid = label
                 , advTime = parseTime defaultCharTime ys 
                 , contents = ys }
        where q = listToRDFGraph  $
                  [ arc label typeRes advancementType,
                    arc label (Var "property") (Var "value"),
                    arc (Var "property") typeRes armViewProperty,
                    arc (Var "property") labelRes (Var "label") ]
              vb = Q.rdfQueryFind g q
              ys = map keypairFromBinding vb

