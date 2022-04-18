module ArM.Advancement where

import Swish.RDF.Graph 
import Swish.RDF.Query as Q
import ArM.Resources 
import ArM.Query 

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

qs c = qparse $ prefixes 
      ++ "?id rdf:type arm:IngameCharacterAdvancement ; "
      ++ " ?property ?value ; "
      ++ " <https://hg.schaathun.net/armchar/schema#advanceCharacter> " ++ c ++ " . "
      ++ "?property rdfs:label ?label . "

qt s = qparse $ prefixes 
      ++ s ++ " arm:advanceTrait ?s . " 
      ++ "?s ?property ?value . "
      ++ "?property rdfs:label ?label . "

-- getSeason g c =  rdfQueryFind (qs c) g
getLogQuads :: RDFGraph -> String -> [Quad]
getLogQuads g c = map quadFromBinding $ rdfQueryFind (qs c) g

data Season = Season {
    year :: Maybe Int,
    season :: Maybe String,
    rdfid :: Maybe RDFLabel,
    contents :: [Triple]
   }
defaultSeason = Season { year = Nothing,
                season = Nothing, rdfid = Nothing, contents = [] }

toSeason :: [Quad] -> Season
toSeason [] = defaultSeason 
toSeason xs = defaultSeason { rdfid = Just $ qfst $ head xs,
         year = getYear ys,
         season = getSeason ys,
         contents = ys }
         where ys = toTripleList xs 

qfst (a,b,c,d) = a
toTriple :: Quad -> Triple
toTriple (a,b,c,d) = (b,c,d)
toTripleList :: [Quad] -> [Triple]
toTripleList = map toTriple
getYear [] = Nothing
getYear ((x,y,z):xs) 
   | y == "Year"  = Just $ read z
   | otherwise    = getYear xs
getSeason [] = Nothing
getSeason ((x,y,z):xs) 
   | y == "Season"  = Just z
   | otherwise      = getSeason xs
