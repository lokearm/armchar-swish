-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.CharacterSheet
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Datatypes and Queries for Character Sheets
--
-----------------------------------------------------------------------------

module ArM.Advancement where

import Swish.RDF.Graph 
import Swish.RDF.Query as Q
import ArM.Resources 
import ArM.Query 
import Data.Maybe 


qt :: String -> RDFGraph
qt s = qparse $ prefixes 
      ++ s ++ " <https://hg.schaathun.net/armchar/schema#Trait> ?id . " 
      ++ "?id ?property ?value . "
      ++ "?property rdfs:label ?label . "


