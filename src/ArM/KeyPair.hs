{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.KeyPair
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Data Types to handle generic queries
--
-----------------------------------------------------------------------------
module ArM.KeyPair where

import Swish.RDF.Parser.N3 (parseN3fromText)
import Swish.RDF.Graph (RDFLabel)
import qualified Swish.RDF.VarBinding as VB 
import Network.URI (URI)
import Swish.VarBinding  (vbMap)
import Data.Maybe  (fromJust)
import Data.List (sort)
import ArM.Resources
import qualified ArM.Query as AQ


-- | `KeyValuePair` represents a key/value pair in JSON jargon
-- or a property/object pair in RDF.  It is designed to hold
-- arbitrary data read from the source format.
data KeyValuePair = KeyValuePair RDFLabel RDFLabel
     deriving (Show,Eq,Ord)

-- | `ObjectKeyValue` represents an RDF triple.  It is used
-- as an intermediate container before converting into a format
-- using `KeyValuePair`.  
-- It may be redundant, as it is functionally equivalent to `RDFTriple`.
-- It is retained in case we want to add additional redundant information
-- such as the contents of rdfs:label-s in the future.
data ObjectKeyValue = ObjectKeyValue RDFLabel RDFLabel RDFLabel
     deriving (Show,Eq,Ord)

sameKey :: ObjectKeyValue -> ObjectKeyValue -> Bool
sameKey (ObjectKeyValue a _ _) (ObjectKeyValue b _ _) = a == b

data KeyValuePairString = KeyValuePairString RDFLabel String
data ObjectKeyValueString = ObjectKeyValueString RDFLabel RDFLabel String

toKeyPair :: ObjectKeyValue -> KeyValuePair 
toKeyPair (ObjectKeyValue a b c) = (KeyValuePair b c)
toKeyPairList :: [ObjectKeyValue] -> [KeyValuePair]
toKeyPairList = map toKeyPair


-- | Split a list of ObjectKeyValue-s so that pairs belonging
-- to the same resource, -- as defined by the first element,
-- are place in the same constituent list.
keypairSplit :: [ObjectKeyValue] -> [[ObjectKeyValue]]
keypairSplit xs = fst $ keypairSplit' ([],sort xs)

-- | keypairSplit' is a mere auxiliary for 'keypairSplit'
keypairSplit' :: ([[ObjectKeyValue]], [ObjectKeyValue]) 
           -> ([[ObjectKeyValue]], [ObjectKeyValue]) 
keypairSplit' (xs,[]) = (xs,[])
keypairSplit' ([],y:ys) = keypairSplit' ([[y]],ys)
keypairSplit' ((x:xs):xss,y:ys) 
    | sameKey  x y = keypairSplit' ((y:x:xs):xss, ys)
    | otherwise    = keypairSplit' ([y]:(x:xs):xss, ys)

-- | Map variable bindings to triples of (property,label,value)
-- Three variables should be bound, property, label, and value.
keypairFromBinding :: VB.RDFVarBinding -> KeyValuePair
keypairFromBinding = f . AQ.metadataFromBinding 
     where 
       f (p,label,value) = KeyValuePair (fromJust p) (fromJust value) 

-- | Map variable bindings to quads of (id,property,label,value)
-- Three variables should be bound, id, property, label, and value.
objectFromBinding :: VB.RDFVarBinding -> ObjectKeyValue
objectFromBinding = f . AQ.quadVB 
     where f (id,p,_,value) = ObjectKeyValue 
              (fromJust id) (fromJust p) (fromJust value) 

getProperty :: RDFLabel -> [KeyValuePair] -> Maybe RDFLabel
getProperty _ [] = Nothing
getProperty k' (KeyValuePair k v:xs) 
   | k' == k  = Just v
   | otherwise      = getProperty k' xs
