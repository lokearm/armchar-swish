module ArM.Advancement where

import Swish.RDF.Graph 
import Swish.RDF.Query as Q
import ArM.Resources 
import ArM.Query 
import Data.Maybe 

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

getLogQuads :: RDFGraph -> String -> [Quad]
getLogQuads g c = map quadFromBinding $ rdfQueryFind (qs c) g
   where
     qs c = qparse $ prefixes 
       ++ "?id rdf:type arm:IngameCharacterAdvancement ; "
       ++ " ?property ?value ; "
       ++ " <https://hg.schaathun.net/armchar/schema#advanceCharacter> "
       ++ c ++ " . "
       ++ "?property rdfs:label ?label . "

getAdvancements :: RDFGraph -> String -> [Advancement]
getAdvancements g = fixAdvancements g . getAdvancements' g 

getAdvancements' :: RDFGraph -> String -> [Advancement]
getAdvancements' g = map toAdvancement . quadSplit . getLogQuads g

fixAdvancements :: RDFGraph -> [Advancement] -> [Advancement]
fixAdvancements g adv = map (fixAdv g) adv
fixAdv :: RDFGraph -> Advancement -> Advancement
fixAdv g a = a { traits = getTraits q g }
    where qt' = qt . show . fromJust . rdfid 
          q = qt' a
getTraits q g = map toTraitAdvancement $ quadSplit $ map quadFromBinding $ rdfQueryFind q g 

-- | TraitAdvancement Resource
data TraitAdvancement = TraitAdvancement {
    traitID :: Maybe RDFLabel,
    traitContents :: [Triple]
   } deriving (Eq, Show)
--
-- | Make a TraitAdvancement object from a list of Quads
toTraitAdvancement :: [Quad] -> TraitAdvancement
toTraitAdvancement [] = TraitAdvancement {
         traitID = Nothing,
         traitContents = [] }
toTraitAdvancement xs = TraitAdvancement { 
         traitID = Just $ qfst $ head xs,
         traitContents = ys }
         where ys = toTripleList xs 


-- | CharacterAdvancement Resource
-- Key computational features are extracted in separate constructors.
-- TraitAdvancements are represented as a list of Quads.
-- Other properties are listed as 'contents'.
data Advancement = Advancement {
    year :: Maybe Int,
    season :: Maybe String,
    rdfid :: Maybe RDFLabel,
    contents :: [Triple],
    traits :: [TraitAdvancement]
   } deriving Eq
defaultAdvancement = Advancement { year = Nothing,
                season = Nothing, rdfid = Nothing, 
                contents = [], traits = [] }
instance Show Advancement where
   show a = "**" ++ s (season a) ++ " " ++ y (year a) ++ "**\n" 
                 ++ sc (contents a) 
                 ++ show (traits a) 
                 ++ "\n"
      where 
         y Nothing = ""
         y (Just x) = show x
         s Nothing = ""
         s (Just x) = x
         sc [] = ""
         sc ((_,x,y):xs) = x ++ ": " ++ y ++ "\n" ++ sc xs
         st [] = ""
         st ((x,_,y,z):xs) = "  " ++ show x ++ ": " ++ y ++ " - " ++ z 
                                  ++  "\n" ++ st xs
instance Ord Advancement where
   compare x y | year x < year y = LT
               | year x > year y = GT
               | sno x < sno y = LT
               | sno x > sno y = GT
               | rdfid x < rdfid y = LT
               | rdfid x > rdfid y = GT
               | contents x < contents y = LT
               | contents x > contents y = GT
               where sno = seasonNo . season

seasonNo Nothing = 0
seasonNo (Just "Spring") = 1
seasonNo (Just "Summer") = 2
seasonNo (Just "Autumn") = 3
seasonNo (Just "Winter") = 4
seasonNo (Just _) = 10

-- | Make an Advancement object from a list of Quads
toAdvancement :: [Quad] -> Advancement
toAdvancement [] = defaultAdvancement 
toAdvancement xs = defaultAdvancement { rdfid = Just $ qfst $ head xs,
         year = getYear ys,
         season = getSeason ys,
         contents = ys }
         where ys = toTripleList xs 


-- | Return the first element of a Quad
qfst :: (a,b,c,d) -> a
qfst (a,b,c,d) = a

-- | Remove the first element of a Quad
toTriple :: Quad -> Triple
toTriple (a,b,c,d) = (b,c,d)

-- | Remove the first element from each Quad in a list
toTripleList :: [Quad] -> [Triple]
toTripleList = map toTriple

-- | Get the year from a list of Triples belonging to an Advancement
getYear [] = Nothing
getYear ((x,y,z):xs) 
   | y == "Year"  = Just $ read z
   | otherwise    = getYear xs
-- | Get the season from a list of Triples belonging to an Advancement
getSeason [] = Nothing
getSeason ((x,y,z):xs) 
   | y == "Season"  = Just z
   | otherwise      = getSeason xs
