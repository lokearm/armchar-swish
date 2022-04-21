{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Metadata
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Handling character metadata
--
-----------------------------------------------------------------------------

module ArM.Metadata (
     getCharacterMetadata ) where

import ArM.Resources
import ArM.Query

import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import Swish.RDF.VarBinding as VB 
import Swish.VarBinding 
-- import Network.URI (URI)


-- | Construct the query for a given character 'c', for use
-- with the following functions .
query c = qparse $  prefixes
     ++ " " ++ c ++ " ?property ?value . "
     ++ " ?property rdf:type  arm:CharacterProperty . "
     ++ " ?property rdfs:label ?label . "

-- | Make a list of metadata, where each data item is
-- a triple consisting of URI, Label, and Value.
-- The inputs are an 'RDFGraph' g and a string naming an RDF resource,
-- either as a prefixed name or as a full URI in angled brackets (<uri>).
getCharacterMetadata :: G.RDFGraph -> String -> [Triple]
getCharacterMetadata g = 
      (map triplesFromBinding) 
      . ( getCharacterMetadataVB g )

-- | Get the variable bindings from the graph.
getCharacterMetadataVB :: G.RDFGraph -> String -> [VB.RDFVarBinding]
getCharacterMetadataVB g c = Q.rdfQueryFind (query c) g

