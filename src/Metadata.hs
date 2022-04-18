module Metadata where

import Resources
import AuxPure

import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import Swish.RDF.VarBinding as VB 
import Swish.VarBinding 

import Data.Maybe

query c = qparse $  prefixes
     ++ " " ++ c ++ " ?p ?value . "
     ++ " ?p rdf:type  arm:CharacterProperty . "
     ++ " ?p rdfs:label ?label . "


getCharacterMetadataVB :: G.RDFGraph -> String -> [VB.RDFVarBinding]
getCharacterMetadataVB g c = Q.rdfQueryFind (query c) g

getCharacterMetadata :: G.RDFGraph -> String -> [(String,String,String)]
getCharacterMetadata = getCharacterMetadataVB 
-- TODO

metadataFromBinding :: VB.RDFVarBinding 
                 -> (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
metadataFromBinding vb = (vbMap vb (G.Var "p"),
                          vbMap vb (G.Var "label"),
                          vbMap vb (G.Var "value"))
metadataFromLabels :: (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
                  -> (String, String, String)
metadataFromLabels (p,label,value) =
            (labelToString p, labelToString label, labelToString value) 

labelToString :: Maybe RDFLabel -> String
labelToString = fromJust . fromRDFLabel . fromJust

