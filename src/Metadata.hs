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

import Resources
import AuxPure

import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import Swish.RDF.VarBinding as VB 
import Swish.VarBinding 
import Network.URI (URI)

import Data.Maybe

query c = qparse $  prefixes
     ++ " " ++ c ++ " ?p ?value . "
     ++ " ?p rdf:type  arm:CharacterProperty . "
     ++ " ?p rdfs:label ?label . "

-- | Make a list of metadata, where each data item is
-- a triple consisting of URI, Label, and Value.
-- The inputs are an 'RDFGraph' g and a string naming an RDF resource,
-- either as a prefixed name or as a full URI in angled brackets (<uri>).
getCharacterMetadata :: G.RDFGraph -> String -> [(URI,String,String)]
getCharacterMetadata g = 
      (map (metadataFromLabels . metadataFromBinding)) 
      . ( getCharacterMetadataVB g )

-- | Step 1.  Get the variable bindings from the graph.
getCharacterMetadataVB :: G.RDFGraph -> String -> [VB.RDFVarBinding]
getCharacterMetadataVB g c = Q.rdfQueryFind (query c) g

-- | Step 2.  Map the variable bindings int MayeBe RDFLabel.
metadataFromBinding :: VB.RDFVarBinding 
                 -> (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
metadataFromBinding vb = (vbMap vb (G.Var "p"),
                          vbMap vb (G.Var "label"),
                          vbMap vb (G.Var "value"))

-- | Step 3.  Mape the RDFLabels into URIs and Strings.
metadataFromLabels :: (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
                  -> (URI, String, String)
metadataFromLabels (p,label,value) =
            (labelToURI p, labelToString label, labelToString value) 

-- | Step 3a.  Map RDFLabel to URI
labelToURI :: Maybe RDFLabel -> URI
labelToURI = fromJust . fromRDFLabel . fromJust

-- | Step 3a.  Map RDFLabel to String
-- Integers and Strings are handled.  
-- Other datatypes will cause error. 
labelToString :: Maybe RDFLabel -> String
labelToString = fromJust .  labelToMaybeString 
labelToMaybeString :: Maybe RDFLabel -> Maybe String
labelToMaybeString ml = case (n) of 
             (Nothing) -> fromRDFLabel l 
             (Just nn) -> Just $ show nn
    where l = fromJust ml
          n =  fromRDFLabel l :: Maybe Int


