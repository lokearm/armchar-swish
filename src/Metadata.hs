module Metadata (
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

getCharacterMetadata :: G.RDFGraph -> String -> [(URI,String,String)]
getCharacterMetadata g = 
      (map (metadataFromLabels . metadataFromBinding)) 
      . ( getCharacterMetadataVB g )
-- TODO

getCharacterMetadataVB :: G.RDFGraph -> String -> [VB.RDFVarBinding]
getCharacterMetadataVB g c = Q.rdfQueryFind (query c) g


metadataFromBinding :: VB.RDFVarBinding 
                 -> (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
metadataFromBinding vb = (vbMap vb (G.Var "p"),
                          vbMap vb (G.Var "label"),
                          vbMap vb (G.Var "value"))
metadataFromLabels :: (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
                  -> (URI, String, String)
metadataFromLabels (p,label,value) =
            (labelToURI p, labelToString label, labelToString value) 

labelToURI :: Maybe RDFLabel -> URI
labelToURI = fromJust . fromRDFLabel . fromJust
labelToString :: Maybe RDFLabel -> String
labelToString = fromJust .  labelToMaybeString 
labelToMaybeString :: Maybe RDFLabel -> Maybe String
labelToMaybeString ml = case (n) of 
             (Nothing) -> fromRDFLabel l 
             (Just nn) -> Just $ show nn
    where l = fromJust ml
          n =  fromRDFLabel l :: Maybe Int


