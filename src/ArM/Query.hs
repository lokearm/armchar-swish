{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text.Lazy as T
import Swish.RDF.VarBinding as VB 
import Network.URI (URI)
import Swish.VarBinding  (vbMap)
import Data.Maybe
import Data.List (sort)
import ArM.Resources


data KeyValuePair = KeyValuePair RDFLabel RDFLabel
data ObjectKeyValue = ObjectKeyValue RDFLabel RDFLabel RDFLabel

data KeyValuePairString = KeyValuePair RDFLabel RDFLabel
data ObjectKeyValueString = ObjectKeyValue RDFLabel RDFLabel RDFLabel

type Triple = (RDFLabel, String, RDFLabel)
type Quad = (RDFLabel,RDFLabel, String, RDFLabel)

-- | Return the first element of a Quad
qfst :: (a,b,c,d) -> a
qfst (a,b,c,d) = a

-- | Remove the first element of a Quad
toTriple :: Quad -> Triple
toTriple (a,b,c,d) = (b,c,d)

-- | Remove the first element from each Quad in a list
toTripleList :: [Quad] -> [Triple]
toTripleList = map toTriple

-- | Check if two Quads match in the first element.
sameID :: Quad -> Quad -> Bool
sameID (x,_,_,_) (y,_,_,_) = x == y

-- | Split a list of Quads so that quads who belong to the same resource,
-- as defined by the first element, are place in the same constituent
-- list.
quadSplit :: [Quad] -> [[Quad]]
quadSplit xs = fst $ quadSplit' ([],sort xs)

-- | quadSplit' is a mere auxiliary for 'quadSplit'
quadSplit' :: ([[Quad]], [Quad]) -> ([[Quad]], [Quad]) 
quadSplit' (xs,[]) = (xs,[])
quadSplit' ([],y:ys) = quadSplit' ([[y]],ys)
quadSplit' ((x:xs):xss,y:ys) 
    | sameID  x y = quadSplit' ((y:x:xs):xss, ys)
    | otherwise   = quadSplit' ([y]:(x:xs):xss, ys)

-- | Create a query graph from an N3 string.
qparse :: String -> RDFGraph
qparse = either error id . parseN3fromText . T.pack

-- | Map variable bindings to triples of (property,label,value)
-- Three variables should be bound, property, label, and value.
triplesFromBinding :: VB.RDFVarBinding -> Triple
triplesFromBinding = metadataFromLabels . metadataFromBinding 

-- | Step 1. Map the variable bindings to Maybe RDFLabel
metadataFromBinding :: VB.RDFVarBinding 
                 -> (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
metadataFromBinding vb = (vbMap vb (G.Var "property"),
                          vbMap vb (G.Var "label"),
                          vbMap vb (G.Var "value"))

-- | Step 2.  Map the RDFLabels into URIs and Strings.
metadataFromLabels :: (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
                  -> Triple
metadataFromLabels (p,label,value) =
            (fromJust p, labelToString label, fromJust value) 

-- | Step 2a.  Map RDFLabel to URI
labelToURI :: Maybe RDFLabel -> URI
labelToURI = fromJust . fromRDFLabel . fromJust

-- | Step 2a.  Map RDFLabel to String
-- Integers and Strings are handled.  
-- Other datatypes will cause error. 
labelToString :: Maybe RDFLabel -> String
labelToString ml = 
    case (n) of 
       (Nothing) -> show l 
       (Just nn) -> nn
    where l = fromJust ml 
          n = fromRDFLabel l :: Maybe String


-- | Map variable bindings to quads of (id,property,label,value)
-- Three variables should be bound, id, property, label, and value.
quadFromBinding :: VB.RDFVarBinding -> Quad
quadFromBinding = quadFromLabels . quadVB 

quadVB :: VB.RDFVarBinding 
        -> (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
quadVB vb = (vbMap vb (G.Var "id"),
             vbMap vb (G.Var "property"),
             vbMap vb (G.Var "label"),
             vbMap vb (G.Var "value"))
quadFromLabels ::
    (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
    -> Quad
quadFromLabels (id,p,label,value) =
    (fromJust id,fromJust p, labelToString label, fromJust value) 
