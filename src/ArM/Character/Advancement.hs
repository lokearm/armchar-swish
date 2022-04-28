{-# LANGUAGE OverloadedStrings #-}

module ArM.Character.Advancement ( Advancement(..)
                                 , getPregameAdvancements
                                 , getIngameAdvancements
                                 , advancementIDstring
                                 , defaultAdvancement
                                 , getSeason
                                 , getYear
                                 , getSortIndex
                                 , toAdvancement
                                 ) where

import Swish.RDF.Graph 
import Swish.RDF.Query as Q
import ArM.Resources 
import ArM.KeyPair 
import Data.Maybe 
import Data.List
import Data.Set (fromList)
import ArM.Character.Trait
import ArM.Types.Character
import ArM.Rules.Aux

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

qt :: String -> RDFGraph
qt s = qparse $ prefixes 
      ++ s ++ " <https://hg.schaathun.net/armchar/schema#advanceTrait> ?id . " 
      ++ "?id ?property ?value . "
      ++ "?property rdfs:label ?label . "


-- | Get a list of all Pregame Advancements of a character.
getPregameAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getPregameAdvancements g c = getAdvancements g $ queryGraph preGameAdv c
   where preGameAdv = Res $ makeSN "PregameAdvancement"

-- | Get a list of all Ingame Advancements of a character.
getIngameAdvancements :: RDFGraph -> RDFLabel -> [Advancement]
getIngameAdvancements g c = getAdvancements g $ queryGraph inGameAdv c
   where inGameAdv = Res $ makeSN "IngameAdvancement"
q1 = queryGraph r
   where r = Res $ makeSN "IngameAdvancement"
q2 = queryGraph r
   where r = Res $ makeSN "PregameAdvancement"

queryGraph :: RDFLabel -> RDFLabel -> RDFGraph
queryGraph c1 = toRDFGraph . fromList . f c1
   where f c1 c2 = [ arc (Var "id") typeRes c1,
            arc (Var "id") (Var "property") (Var "value"),
            arc (Var "id") (Res $ makeSN "advanceCharacter") c2,
            arc (Var "property") labelRes (Var "label") ]

-- | Generic version of 'getIngameAdvancements' and 'getPregameAdvancements'
getAdvancements :: RDFGraph -> RDFGraph -> [Advancement]
getAdvancements g = fixAdvancements g . 
               map toAdvancement . keypairSplit . getGenQuads g 

-- | Auxiliary for 'getAdvancements'
getGenQuads :: RDFGraph -> RDFGraph -> [ObjectKeyValue]
getGenQuads g q = map objectFromBinding $ rdfQueryFind q g

-- | Auxiliary for 'getAdvancements'
fixAdvancements :: RDFGraph -> [Advancement] -> [Advancement]
fixAdvancements g adv = map (fixAdv g) adv

-- | Auxiliary for 'fixAdvancements'
fixAdv :: RDFGraph -> Advancement -> Advancement
fixAdv g a = a { traits = sort $ getTraits q g }
    where qt' = qt . advancementIDstring
          q = qt' a
          getTraits q g = map toTrait
               $ keypairSplit $ map objectFromBinding $ rdfQueryFind q g 

advancementIDstring = show . rdfid 


-- | Make an Advancement object from a list of Quads
toAdvancement :: [ObjectKeyValue] -> Advancement
toAdvancement [] = defaultAdvancement 
toAdvancement xs = defaultAdvancement { rdfid = getkey xs,
         year = getYear ys,
         season = getSeason ys,
         advSortIndex = getSortIndex ys,
         contents = ys }
         where ys = toKeyPairList xs 
               getkey [] = noSuchAdvancement
               getkey (x:xs) = okvSubj x

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
        where q = toRDFGraph . fromList $
                  [ arc label typeRes advancementType,
                    arc label (Var "property") (Var "value"),
                    arc (Var "property") labelRes (Var "label") ]
              vb = Q.rdfQueryFind g q
              ys = map keypairFromBinding vb
