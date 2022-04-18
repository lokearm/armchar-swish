-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Query
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Auxiliary Functions to handle queries
--
-----------------------------------------------------------------------------
module ArM.Query where

import Swish.RDF.Parser.N3 (parseN3fromText)
import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import Data.Text.Lazy as T
import Swish.RDF.VarBinding as VB 
import Network.URI (URI)
import Swish.VarBinding  (vbMap)
import Data.Maybe

-- | Create a query graph from an N3 string.
qparse :: String -> RDFGraph
qparse = either error id . parseN3fromText . T.pack

-- | Map variable bindings to triples of (property,label,value)
-- Three variables should be bound, property, label, and value.
triplesFromBinding :: VB.RDFVarBinding -> (URI, String, String)
triplesFromBinding = metadataFromLabels . metadataFromBinding 

-- | Step 1. Map the variable bindings to Maybe RDFLabel
metadataFromBinding :: VB.RDFVarBinding 
                 -> (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
metadataFromBinding vb = (vbMap vb (G.Var "property"),
                          vbMap vb (G.Var "label"),
                          vbMap vb (G.Var "value"))

-- | Step 2.  Map the RDFLabels into URIs and Strings.
metadataFromLabels :: (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
                  -> (URI, String, String)
metadataFromLabels (p,label,value) =
            (labelToURI p, labelToString label, labelToString value) 

-- | Step 2a.  Map RDFLabel to URI
labelToURI :: Maybe RDFLabel -> URI
labelToURI = fromJust . fromRDFLabel . fromJust

-- | Step 2a.  Map RDFLabel to String
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
