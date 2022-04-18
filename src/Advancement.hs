module Advancement where

import Swish.RDF.Query as Q

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
      ++ "?a arm:advanceCharacter " ++ c ++ " . "
      ++ "?a rdf:type arm:PregameCharacterAdvancement . "
      ++ "?a ?p ?o . "

qt s = qparse $ prefixes 
      ++ s ++ " arm:advanceTrait ?s . " 
      ++ "?s ?p ?o . "
