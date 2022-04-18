module Metadata where

import Resources
import AuxPure


query c = qparse $ prefixes
     ++ " " ++ c ++ " ?p ?v . "
     ++ " ?p rdf:type  arm:CharacterProperty . "
     ++ " ?p rdfs:label ?l . "

         
