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
-- Traits
--    arm:advanceTraitList ( [ a armr:herbam ; arm:addedXP 21 ]  ) ;

qs c = qparse $ prefixes ++ "?a arm:hasCharacter " ++ c ++
       " . ?a ?p ?o ."
