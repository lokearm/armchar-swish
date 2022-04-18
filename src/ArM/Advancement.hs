module ArM.Advancement where

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
getSeason g c = map quadFromBinding $ rdfQueryFind (qs c) g
-- TODO: advancementType is an ObjectProperty (e.g. arm:Reading)

