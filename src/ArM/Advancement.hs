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

getLogQuads :: RDFGraph -> String -> [Quad]
getLogQuads g c = map quadFromBinding $ rdfQueryFind (qs c) g
getAdvancements :: RDFGraph -> String -> [Advancement]
getAdvancements g = map toAdvancement . quadSplit . getLogQuads g 

data Advancement = Advancement {
    year :: Maybe Int,
    season :: Maybe String,
    rdfid :: Maybe RDFLabel,
    contents :: [Triple]
   }
instance Show Advancement where
   show a = "**" ++ s (season a) ++ " " ++ y (year a) ++ "**\n" 
                 ++ sc (contents a) ++ "\n"
      where 
         y Nothing = ""
         y (Just x) = show x
         s Nothing = ""
         s (Just x) = x
         sc [] = ""
         sc ((_,x,y):xs) = x ++ ": " ++ y ++ "\n" ++ sc xs
defaultAdvancement = Advancement { year = Nothing,
                season = Nothing, rdfid = Nothing, contents = [] }

toAdvancement :: [Quad] -> Advancement
toAdvancement [] = defaultAdvancement 
toAdvancement xs = defaultAdvancement { rdfid = Just $ qfst $ head xs,
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
